from selenium import webdriver
from bs4 import BeautifulSoup
from selenium.webdriver.support.ui import WebDriverWait
import time

def update_page_to_crawl():
    time.sleep(0.5)
    html = driver.page_source
    return BeautifulSoup(html, 'html.parser')

driver = webdriver.PhantomJS(executable_path='/Users/Harry/Downloads/phantomjs-2.1.1-macosx/bin/phantomjs')
driver.get("https://play.google.com/store/apps/details?id=com.netflix.mediaclient&hl=en&showAllReviews=true")

itemsLoaded = 0

soup = update_page_to_crawl()

x_upper = 4

for y in range(0, 18):
    for x in range(0, x_upper):
        try:
            driver.execute_script("window.scrollTo(0, document.body.scrollHeight)")
            # fullReviewbutton = driver.find_element_by_xpath("//button[@jsname='gxjVle']")
            # print("fullReviewbutton: ")
            # print(fullReviewbutton)   
            # fullReviewbutton.click()
            soup = update_page_to_crawl()
        except Exception as e:
            print(e)
            x_upper = x_upper + 1
            break
    try:
        print('[Show More] clicked!\n')
        nextButton = driver.find_element_by_xpath('//*[@id="fcxH9b"]/div[4]/c-wiz/div/div[2]/div/div[1]/div/div/div/div[2]/div[2]/div')
        nextButton.click()
        soup = update_page_to_crawl()    
    except Exception as e:
        print(e)
        x_upper = x_upper + 1


file = open("testfile.txt","w") 
print('\n\n\n\n')
for e in soup.findAll("span", attrs={'jsname':'bN97Pc'}):
    review = (e.text).replace('\n','')
    if "...Full Review" not in review:
        print(review)
        print('\n\n')
        itemsLoaded = itemsLoaded + 1
        if itemsLoaded == 2000:
            break
        file.write(review)
        file.write("\n\n")


print('loaded ' + str(itemsLoaded) + ' reviews')

file.close() 





# <div class="cQj82c">
#     <button class="LkLjZd ScJHi OzU4dc  " jsaction="click:TiglPc" jsname="gxjVle">Full Review
#     </button>
# </div>