<!DOCTYPE html>
<!-- Generated by pkgdown: do not edit by hand --><html lang="en"><head><meta http-equiv="Content-Type" content="text/html; charset=UTF-8"><meta charset="utf-8"><meta http-equiv="X-UA-Compatible" content="IE=edge"><meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"><meta name="description" content="Summarize the age-to-age factors resulting from a 
  call to the ata function."><title>Summary method for object of class 'ata' — summary.ata • ChainLadder</title><!-- favicons --><link rel="icon" type="image/png" sizes="16x16" href="../favicon-16x16.png"><link rel="icon" type="image/png" sizes="32x32" href="../favicon-32x32.png"><link rel="apple-touch-icon" type="image/png" sizes="180x180" href="../apple-touch-icon.png"><link rel="apple-touch-icon" type="image/png" sizes="120x120" href="../apple-touch-icon-120x120.png"><link rel="apple-touch-icon" type="image/png" sizes="76x76" href="../apple-touch-icon-76x76.png"><link rel="apple-touch-icon" type="image/png" sizes="60x60" href="../apple-touch-icon-60x60.png"><script src="../deps/jquery-3.6.0/jquery-3.6.0.min.js"></script><meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"><link href="../deps/bootstrap-5.1.3/bootstrap.min.css" rel="stylesheet"><script src="../deps/bootstrap-5.1.3/bootstrap.bundle.min.js"></script><!-- Font Awesome icons --><link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.12.1/css/all.min.css" integrity="sha256-mmgLkCYLUQbXn0B1SRqzHar6dCnv9oZFPEC1g1cwlkk=" crossorigin="anonymous"><link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.12.1/css/v4-shims.min.css" integrity="sha256-wZjR52fzng1pJHwx4aV2AO3yyTOXrcDW7jBpJtTwVxw=" crossorigin="anonymous"><!-- bootstrap-toc --><script src="https://cdn.rawgit.com/afeld/bootstrap-toc/v1.0.1/dist/bootstrap-toc.min.js"></script><!-- headroom.js --><script src="https://cdnjs.cloudflare.com/ajax/libs/headroom/0.11.0/headroom.min.js" integrity="sha256-AsUX4SJE1+yuDu5+mAVzJbuYNPHj/WroHuZ8Ir/CkE0=" crossorigin="anonymous"></script><script src="https://cdnjs.cloudflare.com/ajax/libs/headroom/0.11.0/jQuery.headroom.min.js" integrity="sha256-ZX/yNShbjqsohH1k95liqY9Gd8uOiE1S4vZc+9KQ1K4=" crossorigin="anonymous"></script><!-- clipboard.js --><script src="https://cdnjs.cloudflare.com/ajax/libs/clipboard.js/2.0.6/clipboard.min.js" integrity="sha256-inc5kl9MA1hkeYUt+EC3BhlIgyp/2jDIyBLS6k3UxPI=" crossorigin="anonymous"></script><!-- search --><script src="https://cdnjs.cloudflare.com/ajax/libs/fuse.js/6.4.6/fuse.js" integrity="sha512-zv6Ywkjyktsohkbp9bb45V6tEMoWhzFzXis+LrMehmJZZSys19Yxf1dopHx7WzIKxr5tK2dVcYmaCk2uqdjF4A==" crossorigin="anonymous"></script><script src="https://cdnjs.cloudflare.com/ajax/libs/autocomplete.js/0.38.0/autocomplete.jquery.min.js" integrity="sha512-GU9ayf+66Xx2TmpxqJpliWbT5PiGYxpaG8rfnBEk1LL8l1KGkRShhngwdXK1UgqhAzWpZHSiYPc09/NwDQIGyg==" crossorigin="anonymous"></script><script src="https://cdnjs.cloudflare.com/ajax/libs/mark.js/8.11.1/mark.min.js" integrity="sha512-5CYOlHXGh6QpOFA/TeTylKLWfB3ftPsde7AnmhuitiTX4K5SqCLBeKro6sPS8ilsz1Q4NRx3v8Ko2IBiszzdww==" crossorigin="anonymous"></script><!-- pkgdown --><script src="../pkgdown.js"></script><meta property="og:title" content="Summary method for object of class 'ata' — summary.ata"><meta property="og:description" content="Summarize the age-to-age factors resulting from a 
  call to the ata function."><meta property="og:image" content="http://mages.github.io/ChainLadder/logo.png"><!-- mathjax --><script src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js" integrity="sha256-nvJJv9wWKEm88qvoQl9ekL2J+k/RWIsaSScxxlsrv8k=" crossorigin="anonymous"></script><script src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/config/TeX-AMS-MML_HTMLorMML.js" integrity="sha256-84DKXVJXs0/F8OTMzX4UR909+jtl4G7SPypPavF+GfA=" crossorigin="anonymous"></script><!--[if lt IE 9]>
<script src="https://oss.maxcdn.com/html5shiv/3.7.3/html5shiv.min.js"></script>
<script src="https://oss.maxcdn.com/respond/1.4.2/respond.min.js"></script>
<![endif]--></head><body>
    <a href="#main" class="visually-hidden-focusable">Skip to contents</a>
    

    <nav class="navbar fixed-top navbar-light navbar-expand-lg bg-light"><div class="container">
    
    <a class="navbar-brand me-2" href="../index.html">ChainLadder</a>

    <small class="nav-text text-muted me-auto" data-bs-toggle="tooltip" data-bs-placement="bottom" title="">0.2.16</small>

    
    <button class="navbar-toggler" type="button" data-bs-toggle="collapse" data-bs-target="#navbar" aria-controls="navbar" aria-expanded="false" aria-label="Toggle navigation">
      <span class="navbar-toggler-icon"></span>
    </button>

    <div id="navbar" class="collapse navbar-collapse ms-3">
      <ul class="navbar-nav me-auto"><li class="nav-item">
  <a class="nav-link" href="../articles/ChainLadder.html">Get started</a>
</li>
<li class="active nav-item">
  <a class="nav-link" href="../reference/index.html">Reference</a>
</li>
<li class="nav-item">
  <a class="nav-link" href="../news/index.html">Changelog</a>
</li>
      </ul><form class="form-inline my-2 my-lg-0" role="search">
        <input type="search" class="form-control me-sm-2" aria-label="Toggle navigation" name="search-input" data-search-index="../search.json" id="search-input" placeholder="Search for" autocomplete="off"></form>

      <ul class="navbar-nav"><li class="nav-item">
  <a class="external-link nav-link" href="https://github.com/mages/ChainLadder/" aria-label="github">
    <span class="fab fa fab fa-github fa-lg"></span>
     
  </a>
</li>
      </ul></div>

    
  </div>
</nav><div class="container template-reference-topic">
<div class="row">
  <main id="main" class="col-md-9"><div class="page-header">
      <img src="../logo.png" class="logo" alt=""><h1>Summary method for object of class 'ata'</h1>
      
      <div class="d-none name"><code>summary.ata.rd</code></div>
    </div>

    <div class="ref-description section level2">
    <p>Summarize the age-to-age factors resulting from a 
  call to the <code><a href="ata.html">ata</a></code> function.</p>
    </div>

    <div class="section level2">
    <h2 id="ref-usage">Usage<a class="anchor" aria-label="anchor" href="#ref-usage"></a></h2>
    <div class="sourceCode"><pre class="sourceCode r"><code><span><span class="co"># S3 method for ata</span></span>
<span><span class="fu"><a href="https://rdrr.io/r/base/summary.html" class="external-link">summary</a></span><span class="op">(</span><span class="va">object</span>, digits<span class="op">=</span><span class="fl">3</span>, <span class="va">...</span><span class="op">)</span></span></code></pre></div>
    </div>

    <div class="section level2">
    <h2 id="arguments">Arguments<a class="anchor" aria-label="anchor" href="#arguments"></a></h2>
    <dl><dt>object</dt>
<dd><p>object resulting from a call to <code><a href="ata.html">ata</a></code></p></dd>


    <dt>digits</dt>
<dd><p>integer indicating the number of decimal places
        for rounding the factors.
        The default is 3.
        <code>NULL</code> indicates that rounding should take place.</p></dd>

    <dt>...</dt>
<dd><p>not used</p></dd>


</dl></div>
    <div class="section level2">
    <h2 id="details">Details<a class="anchor" aria-label="anchor" href="#details"></a></h2>
    <p>A call to <code><a href="ata.html">ata</a></code> produces a matrix of age-to-age factors
with two attributes -- 
the simple and volume weighted averages.
<code>summary.ata</code> creates a new matrix with the averages appended
as rows at the bottom.</p>
    </div>
    <div class="section level2">
    <h2 id="value">Value<a class="anchor" aria-label="anchor" href="#value"></a></h2>
    

<p>A <code>matrix</code>.</p>
    </div>
    <div class="section level2">
    <h2 id="author">Author<a class="anchor" aria-label="anchor" href="#author"></a></h2>
    <p>Dan Murphy</p>
    </div>
    <div class="section level2">
    <h2 id="see-also">See also<a class="anchor" aria-label="anchor" href="#see-also"></a></h2>
    <div class="dont-index"><p>See also <code><a href="ata.html">ata</a></code> and <code><a href="print.ata.html">print.ata</a></code></p></div>
    </div>

    <div class="section level2">
    <h2 id="ref-examples">Examples<a class="anchor" aria-label="anchor" href="#ref-examples"></a></h2>
    <div class="sourceCode"><pre class="sourceCode r"><code><span class="r-in"><span><span class="va">y</span> <span class="op">&lt;-</span> <span class="fu"><a href="ata.html">ata</a></span><span class="op">(</span><span class="va">RAA</span><span class="op">)</span></span></span>
<span class="r-in"><span><span class="fu"><a href="https://rdrr.io/r/base/summary.html" class="external-link">summary</a></span><span class="op">(</span><span class="va">y</span>, digits<span class="op">=</span><span class="fl">4</span><span class="op">)</span></span></span>
<span class="r-out co"><span class="r-pr">#&gt;</span>       dev</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> origin     1-2    2-3    3-4    4-5    5-6    6-7    7-8    8-9   9-10</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>   1981  1.6498 1.3190 1.0823 1.1469 1.1951 1.1130 1.0333 1.0029 1.0092</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>   1982 40.4245 1.2593 1.9766 1.2921 1.1318 0.9934 1.0434 1.0331     NA</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>   1983  2.6370 1.5428 1.1635 1.1607 1.1857 1.0292 1.0264     NA     NA</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>   1984  2.0433 1.3644 1.3489 1.1015 1.1135 1.0377     NA     NA     NA</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>   1985  8.7592 1.6556 1.3999 1.1708 1.0087     NA     NA     NA     NA</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>   1986  4.2597 1.8157 1.1054 1.2255     NA     NA     NA     NA     NA</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>   1987  7.2172 2.7229 1.1250     NA     NA     NA     NA     NA     NA</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>   1988  5.1421 1.8874     NA     NA     NA     NA     NA     NA     NA</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>   1989  1.7220     NA     NA     NA     NA     NA     NA     NA     NA</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>   smpl  8.2061 1.6959 1.3145 1.1829 1.1270 1.0433 1.0344 1.0180 1.0092</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>   vwtd  2.9994 1.6235 1.2709 1.1717 1.1134 1.0419 1.0333 1.0169 1.0092</span>
<span class="r-in"><span></span></span>
</code></pre></div>
    </div>
  </main><aside class="col-md-3"><nav id="toc"><h2>On this page</h2>
    </nav></aside></div>


    <footer><div class="pkgdown-footer-left">
  <p></p><p>Developed by Markus Gesmann, Daniel Murphy, Yanwei (Wayne) Zhang, Alessandro Carrato, Mario Wuthrich, Fabio Concina, Eric Dal Moro.</p>
</div>

<div class="pkgdown-footer-right">
  <p></p><p>Site built with <a href="https://pkgdown.r-lib.org/" class="external-link">pkgdown</a> 2.0.6.</p>
</div>

    </footer></div>

  

  

  </body></html>

