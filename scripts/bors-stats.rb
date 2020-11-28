#!/usr/bin/env ruby
#
require 'net/http'
require 'uri'
require 'json'
require 'date'
require 'ansi'
require 'thor'

def sendGithubGraphQLQuery(qry)
  githubApiToken = ENV.fetch("GITHUB_API_TOKEN")
  header = { 'Authorization': 'bearer ' + githubApiToken }
  url = "https://api.github.com/graphql"
  uri = URI.parse(url)
  data = {query: qry}
  http = Net::HTTP.new(uri.host, uri.port)
  http.use_ssl = true
  request = Net::HTTP::Post.new(uri.request_uri, header)
  request.body = data.to_json
  return JSON.parse(http.request(request).body)
end

# A parsed bors comment corresponding to a succeeding or failing build
BorsComment = Struct.new(:url, :bodyText, :createdAt, :tags, :succeeded) do
  def to_s
    self.pretty
  end

  def pretty
    return self.pretty_time + "\n" + bodyText
  end
  def pretty_time
    color = (succeeded ? ANSI.green : ANSI.red)
    return color + createdAt.strftime("%d %b %H:%M") + ANSI.clear
  end

  def pretty_tags
    ANSI.yellow + tags.join(", ") + ANSI.clear
  end
end

def fetch_comments
  numberPRsToFetch = 40
  numberCommentsToFetch = 100
  query = <<~END
    query { repository(name: "cardano-wallet", owner: "input-output-hk") {
      pullRequests(last: #{numberPRsToFetch}) { edges { node {
        comments(first: #{numberCommentsToFetch}) { edges { node {
          bodyText,
          createdAt,
          url,
          author {
              login
          }
        }}}
      }}}
    }}
  END
  response = sendGithubGraphQLQuery(query)
  return response['data']['repository']['pullRequests']['edges']
      .map { |x| x['node']['comments']['edges']}
      .flatten
      .map { |x| x['node']}
      .filter { |x| x['author']['login'] == "iohk-bors" }
      .map do |x|
        body = x['bodyText']
        tags = body.scan(/^#[\d\w\-]+/).to_a
        createdAt = DateTime.parse(x['createdAt'])
        succ = x['bodyText'].include? "Build succeeded"
        BorsComment.new(x['url'], body, createdAt, tags, succ)
      end
end

# Fetch github comments with the "Test failure" label, and create a map from
# issue number to title and url
#
# Returns e.g.
# {2083=>[{"number"=>2083, "url"=>"https://github.com/input-output-hk/cardano-wallet/issues/2083", "title"=>"Windows integration" }
def fetch_gh_ticket_titlemap
  query = <<~END
    query { repository(name: "cardano-wallet", owner: "input-output-hk") {
      issues(labels: ["Test failure"], last: 100) { edges { node {
        number,
        url,
        title
      }}}
    }}
  END
  return sendGithubGraphQLQuery(query)['data']['repository']['issues']['edges']
    .map { |x| x['node'] }
    .group_by { |x| "#" + x['number'].to_s }
    .transform_values { |x| x[0] }
end

def show_bors_failures(comments, titlemap)
  comments.each do |c|
    # Only print the full comment if failure or if no tags
    maybeDetails = (c.succeeded or c.tags.length > 0) ? "" : c.bodyText
    #key = c.tags.first
    #title = titlemap.dig key, "title"
    #title = unless title.nil? then title else "" end

    header = c.pretty_time + " " + c.pretty_tags + " " + ANSI.blue + c.url + ANSI.clear

    puts ("\n" + header + "\n" + maybeDetails + "\n")
  end
end

def show_breakdown(comments, tm)
  m = {}
  comments.each do |c|
    c.tags.each do |tag|
      m[tag] = m.fetch(tag, []) + [c]
    end
  end
  return m.collect {|tag,failures| {:tag => tag, :n => failures.length}}.sort_by {|x| x[:n] }.reverse
end


def apply_rules(tags)
  rules = {
    "#2337" => ["integration", "timeout", "STAKE_POOLS_GARBAGE_COLLECTION_01"]
    }
  tags.each do |t|
    unless rules[t].nil?
    then tags += rules[t]
    end
  end
  return tags
end


def bold(s)
  ANSI.bold + s + ANSI.clear
end
def yellow(s)
  ANSI.yellow + s + ANSI.clear
end

$tm = fetch_gh_ticket_titlemap
$comments = fetch_comments #.map { |c| c.tags = apply_rules(c.tags); c }

class BorsStats < Thor
  desc "list", "list all failures with optional filter (e.g. list 2292)"
  def list(tag = nil)
    unless tag.nil? then
      show_bors_failures($comments.filter {|x| x.tags.include? ('#'+tag) }, $tm)
    else
      show_bors_failures($comments, $tm)
    end
  end

  desc "breakdown", "Group failures by tags and list by frequency"
  def breakdown
    show_breakdown($comments, $tm).each do |k,v|
      t = k[:tag]
      n = k[:n]
      nTot = $comments.count
      failureRate = '%.0f%%' % (100.0 * n / nTot)
      title = $tm.dig t, "title"
      title = title.nil? ? "" : title
      puts (bold(n.to_s) + " times (" + bold(failureRate.to_s) + ") " + yellow(t) + " " + bold(title))
    end
  end
end

BorsStats.start(ARGV)
