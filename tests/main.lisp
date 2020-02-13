(defpackage cl-robots/tests/main
  (:use :cl
        :cl-robots
        :rove))
(in-package :cl-robots/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-robots)' in your Lisp.
(let ((*rt-robots* "
User-agent: Googlebot
Disallow: /search*
Disallow: /preview*
Disallow: /register*
Disallow: /login*
Disallow: /requestresetpass*
Disallow: /*/video/
Disallow: /shortcode/
Disallow: /preview/
Disallow: /api/
Disallow: /rtmobile/
Disallow: /listing/
Disallow: /widget/
Disallow: /schedulejson/
Disallow: /vote/
Disallow: *?
Allow: /static/fonts/icon/
Allow: /static/css/
Allow: /static/js/

User-agent: bingbot
Disallow: /search*
Disallow: /preview*
Disallow: /register*
Disallow: /login*
Disallow: /requestresetpass*
Disallow: /*/video/
Disallow: /shortcode/
Disallow: /preview/
Disallow: /api/
Disallow: /rtmobile/
Disallow: /listing/
Disallow: /widget/
Disallow: /schedulejson/
Disallow: /vote/
Disallow: *?
Allow: /static/fonts/icon/
Allow: /static/css/
Allow: /static/js/

User-agent: *
Disallow: /search*
Disallow: /preview*
Disallow: /register*
Disallow: /login*
Disallow: /requestresetpass*
Disallow: /*/video/
Disallow: /shortcode/
Disallow: /preview/
Disallow: /api/
Disallow: /rtmobile/
Disallow: /listing/
Disallow: /widget/
Disallow: /schedulejson/
Disallow: /vote/
Disallow: *?
Allow: /static/fonts/icon/
Allow: /static/css/
Allow: /static/js/

Sitemap: https://www.rt.com/sitemap.xml
Sitemap: https://www.rt.com/newssitemap.xml
"))
  (deftest test-sitemaps
    (testing "should extract two sitemaps"
      (ok 
       (equal
        (list "https://www.rt.com/sitemap.xml" "https://www.rt.com/newssitemap.xml") 
        (sitemaps (read-robots.txt *rt-robots*)))))))
