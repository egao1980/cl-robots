<a id="orge2fa41e"></a>

# cl-robots - simple robots.txt reader


<a id="org1972728"></a>

## Usage

``` common-lisp
(ql:quickload '(:dexador :cl-robots))
    
;; read bbc.co.uk robots.txt 
(defparameter *robots*
  (cl-robots:read-robots.txt (dex:get "http://bbc.co.uk/robots.txt")))

;; get sitemap URLs
(cl-robots:sitemaps *robots*)
```

<a id="org2e77242"></a>

## Installation

``` common-lisp
(ql:quickload :cl-robots)
```


<a id="orgc24dccd"></a>

## Author

-   Nikolai Matiushev (nikolai.matiushev@egao.co.uk)


<a id="orge73d72c"></a>

## Copyright

Copyright (c) 2020 Nikolai Matiushev


<a id="orgdeceb47"></a>

## License

Licensed under the MIT License.

