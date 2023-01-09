<!DOCTYPE html>
<!-- Generated by pkgdown: do not edit by hand --><html lang="en"><head><meta http-equiv="Content-Type" content="text/html; charset=UTF-8"><meta charset="utf-8"><meta http-equiv="X-UA-Compatible" content="IE=edge"><meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"><meta name="description" content="Extract residuals of a MackChainLadder model by 
  origin-, calendar- and development period."><title>Extract residuals of a ChainLadder model — coef.ChainLadder • ChainLadder</title><!-- favicons --><link rel="icon" type="image/png" sizes="16x16" href="../favicon-16x16.png"><link rel="icon" type="image/png" sizes="32x32" href="../favicon-32x32.png"><link rel="apple-touch-icon" type="image/png" sizes="180x180" href="../apple-touch-icon.png"><link rel="apple-touch-icon" type="image/png" sizes="120x120" href="../apple-touch-icon-120x120.png"><link rel="apple-touch-icon" type="image/png" sizes="76x76" href="../apple-touch-icon-76x76.png"><link rel="apple-touch-icon" type="image/png" sizes="60x60" href="../apple-touch-icon-60x60.png"><script src="../deps/jquery-3.6.0/jquery-3.6.0.min.js"></script><meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"><link href="../deps/bootstrap-5.2.2/bootstrap.min.css" rel="stylesheet"><script src="../deps/bootstrap-5.2.2/bootstrap.bundle.min.js"></script><!-- Font Awesome icons --><link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.12.1/css/all.min.css" integrity="sha256-mmgLkCYLUQbXn0B1SRqzHar6dCnv9oZFPEC1g1cwlkk=" crossorigin="anonymous"><link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.12.1/css/v4-shims.min.css" integrity="sha256-wZjR52fzng1pJHwx4aV2AO3yyTOXrcDW7jBpJtTwVxw=" crossorigin="anonymous"><!-- bootstrap-toc --><script src="https://cdn.jsdelivr.net/gh/afeld/bootstrap-toc@v1.0.1/dist/bootstrap-toc.min.js" integrity="sha256-4veVQbu7//Lk5TSmc7YV48MxtMy98e26cf5MrgZYnwo=" crossorigin="anonymous"></script><!-- headroom.js --><script src="https://cdnjs.cloudflare.com/ajax/libs/headroom/0.11.0/headroom.min.js" integrity="sha256-AsUX4SJE1+yuDu5+mAVzJbuYNPHj/WroHuZ8Ir/CkE0=" crossorigin="anonymous"></script><script src="https://cdnjs.cloudflare.com/ajax/libs/headroom/0.11.0/jQuery.headroom.min.js" integrity="sha256-ZX/yNShbjqsohH1k95liqY9Gd8uOiE1S4vZc+9KQ1K4=" crossorigin="anonymous"></script><!-- clipboard.js --><script src="https://cdnjs.cloudflare.com/ajax/libs/clipboard.js/2.0.6/clipboard.min.js" integrity="sha256-inc5kl9MA1hkeYUt+EC3BhlIgyp/2jDIyBLS6k3UxPI=" crossorigin="anonymous"></script><!-- search --><script src="https://cdnjs.cloudflare.com/ajax/libs/fuse.js/6.4.6/fuse.js" integrity="sha512-zv6Ywkjyktsohkbp9bb45V6tEMoWhzFzXis+LrMehmJZZSys19Yxf1dopHx7WzIKxr5tK2dVcYmaCk2uqdjF4A==" crossorigin="anonymous"></script><script src="https://cdnjs.cloudflare.com/ajax/libs/autocomplete.js/0.38.0/autocomplete.jquery.min.js" integrity="sha512-GU9ayf+66Xx2TmpxqJpliWbT5PiGYxpaG8rfnBEk1LL8l1KGkRShhngwdXK1UgqhAzWpZHSiYPc09/NwDQIGyg==" crossorigin="anonymous"></script><script src="https://cdnjs.cloudflare.com/ajax/libs/mark.js/8.11.1/mark.min.js" integrity="sha512-5CYOlHXGh6QpOFA/TeTylKLWfB3ftPsde7AnmhuitiTX4K5SqCLBeKro6sPS8ilsz1Q4NRx3v8Ko2IBiszzdww==" crossorigin="anonymous"></script><!-- pkgdown --><script src="../pkgdown.js"></script><meta property="og:title" content="Extract residuals of a ChainLadder model — coef.ChainLadder"><meta property="og:description" content="Extract residuals of a MackChainLadder model by 
  origin-, calendar- and development period."><meta property="og:image" content="http://mages.github.io/ChainLadder/logo.png"><!-- mathjax --><script src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js" integrity="sha256-nvJJv9wWKEm88qvoQl9ekL2J+k/RWIsaSScxxlsrv8k=" crossorigin="anonymous"></script><script src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/config/TeX-AMS-MML_HTMLorMML.js" integrity="sha256-84DKXVJXs0/F8OTMzX4UR909+jtl4G7SPypPavF+GfA=" crossorigin="anonymous"></script><!--[if lt IE 9]>
<script src="https://oss.maxcdn.com/html5shiv/3.7.3/html5shiv.min.js"></script>
<script src="https://oss.maxcdn.com/respond/1.4.2/respond.min.js"></script>
<![endif]--></head><body>
    <a href="#main" class="visually-hidden-focusable">Skip to contents</a>
    

    <nav class="navbar fixed-top navbar-light navbar-expand-lg bg-light"><div class="container">
    
    <a class="navbar-brand me-2" href="../index.html">ChainLadder</a>

    <small class="nav-text text-muted me-auto" data-bs-toggle="tooltip" data-bs-placement="bottom" title="">0.2.17</small>

    
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
      <img src="../logo.png" class="logo" alt=""><h1>Extract residuals of a ChainLadder model</h1>
      
      <div class="d-none name"><code>coef.ChainLadder.rd</code></div>
    </div>

    <div class="ref-description section level2">
    <p>Extract residuals of a <code><a href="MackChainLadder.html">MackChainLadder</a></code> model by 
  origin-, calendar- and development period.</p>
    </div>

    <div class="section level2">
    <h2 id="ref-usage">Usage<a class="anchor" aria-label="anchor" href="#ref-usage"></a></h2>
    <div class="sourceCode"><pre class="sourceCode r"><code><span><span class="co"># S3 method for ChainLadder</span></span>
<span><span class="fu"><a href="https://rdrr.io/r/stats/coef.html" class="external-link">coef</a></span><span class="op">(</span><span class="va">object</span>, <span class="va">...</span><span class="op">)</span></span></code></pre></div>
    </div>

    <div class="section level2">
    <h2 id="arguments">Arguments<a class="anchor" aria-label="anchor" href="#arguments"></a></h2>
    <dl><dt>object</dt>
<dd><p>output of the <code><a href="chainladder.html">chainladder</a></code> function</p></dd>

  <dt>...</dt>
<dd><p>optional arguments which may become named
    attributes of the resulting vector</p></dd>

</dl></div>
    <div class="section level2">
    <h2 id="value">Value<a class="anchor" aria-label="anchor" href="#value"></a></h2>
    

<p>The function returns a vector of the single-parameter coefficients -- also
  called age-to-age (ATA) or report-to-report (RTR) factors --
  of the models produced by running the 'chainladder' function.</p>
    </div>
    <div class="section level2">
    <h2 id="author">Author<a class="anchor" aria-label="anchor" href="#author"></a></h2>
    <p>Dan Murphy</p>
    </div>
    <div class="section level2">
    <h2 id="see-also">See also<a class="anchor" aria-label="anchor" href="#see-also"></a></h2>
    <div class="dont-index"><p>See Also <code><a href="chainladder.html">chainladder</a></code></p></div>
    </div>

    <div class="section level2">
    <h2 id="ref-examples">Examples<a class="anchor" aria-label="anchor" href="#ref-examples"></a></h2>
    <div class="sourceCode"><pre class="sourceCode r"><code><span class="r-in"><span></span></span>
<span class="r-in"><span><span class="fu"><a href="https://rdrr.io/r/stats/coef.html" class="external-link">coef</a></span><span class="op">(</span><span class="fu"><a href="chainladder.html">chainladder</a></span><span class="op">(</span><span class="va">RAA</span><span class="op">)</span><span class="op">)</span></span></span>
<span class="r-out co"><span class="r-pr">#&gt;</span>        1        2        3        4        5        6        7        8 </span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 2.999359 1.623523 1.270888 1.171675 1.113385 1.041935 1.033264 1.016936 </span>
<span class="r-out co"><span class="r-pr">#&gt;</span>        9 </span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 1.009217 </span>
<span class="r-in"><span></span></span>
</code></pre></div>
    </div>
  </main><aside class="col-md-3"><nav id="toc"><h2>On this page</h2>
    </nav></aside></div>


    <footer><div class="pkgdown-footer-left">
  <p></p><p>Developed by Markus Gesmann, Daniel Murphy, Yanwei (Wayne) Zhang, Alessandro Carrato, Mario Wuthrich, Fabio Concina, Eric Dal Moro.</p>
</div>

<div class="pkgdown-footer-right">
  <p></p><p>Site built with <a href="https://pkgdown.r-lib.org/" class="external-link">pkgdown</a> 2.0.7.</p>
</div>

    </footer></div>

  

  

  </body></html>

