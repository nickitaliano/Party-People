require 'selenium-webdriver'
require "cgi"
profile = Selenium::WebDriver::Firefox::Profile.new
profile["browser.download.folderList"] = 1
profile["browser.helperApps.neverAsk.saveToDisk"] = 'audio/mpeg3,audio/x-mpeg-3,video/mpeg,video/x-mpeg'
driver = Selenium::WebDriver.for :firefox, :profile => profile
driver.manage.timeouts.page_load = 30

#song_list_filepath = Path to file containing names of songs in each line. Foe example:
song_list_filepath = "/Users/nami/Desktop/list_of_songs_i_want.txt"

songs = File.read(song_list_filepath).split("\n")
songs.each do |song_name|
  begin
    query     = CGI.escape(song_name)
    url       = "http://youtube.com/results?search_query="
    page      = driver.navigate.to (url + query)
    ele       = driver.find_elements(:xpath, '//a[contains(@href, "watch?")]')[0]
    video_url = ele.attribute('href')
    driver.navigate.to("http://peggo.co/")
    ele       = driver.find_element(id: 'search-box')
    ele.send_keys(video_url)
    # driver.find_element(id: 'swap').click unless driver.find_element(id:"id3-title").attribute('value').empty? # Uncomment to swap ID3 tags
    driver.find_element(id: 'record-audio').click
    # Uncomment for very slow internet speeds
    # sleep 5
    # driver.find_element(id: 'record-audio').click if driver.find_element(id: 'record-audio').attribute('class') != 'recording'
  rescue
    #p e.message
    p song_name
    sleep 60
    next
  end
end

driver.quit
