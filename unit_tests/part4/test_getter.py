from bs4 import BeautifulSoup as bs

with open("part4tests.html", "r") as html_file:
    html_string = html_file.read()
    soup = bs(html_string, "html.parser")
    tests = soup.find_all("pre")
    print(len(tests))
    for i in range(1, 21):
        f = open(f"unit_test_{i:02d}.txt", "w")
        f.write(tests[i-1].string.strip())
        f.close()