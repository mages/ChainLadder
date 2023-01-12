<!DOCTYPE html>
<!-- Generated by pkgdown: do not edit by hand --><html lang="en"><head><meta http-equiv="Content-Type" content="text/html; charset=UTF-8"><meta charset="utf-8"><meta http-equiv="X-UA-Compatible" content="IE=edge"><meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"><meta name="description" content="Function to compute the covariance matrix of the parameter estimates
for the ClarkLDF and ClarkCapeCod methods."><title>Covariance Matrix of Parameter Estimates -- Clark's methods — vcov.clark • ChainLadder</title><!-- favicons --><link rel="icon" type="image/png" sizes="16x16" href="../favicon-16x16.png"><link rel="icon" type="image/png" sizes="32x32" href="../favicon-32x32.png"><link rel="apple-touch-icon" type="image/png" sizes="180x180" href="../apple-touch-icon.png"><link rel="apple-touch-icon" type="image/png" sizes="120x120" href="../apple-touch-icon-120x120.png"><link rel="apple-touch-icon" type="image/png" sizes="76x76" href="../apple-touch-icon-76x76.png"><link rel="apple-touch-icon" type="image/png" sizes="60x60" href="../apple-touch-icon-60x60.png"><script src="../deps/jquery-3.6.0/jquery-3.6.0.min.js"></script><meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"><link href="../deps/bootstrap-5.2.2/bootstrap.min.css" rel="stylesheet"><script src="../deps/bootstrap-5.2.2/bootstrap.bundle.min.js"></script><!-- Font Awesome icons --><link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.12.1/css/all.min.css" integrity="sha256-mmgLkCYLUQbXn0B1SRqzHar6dCnv9oZFPEC1g1cwlkk=" crossorigin="anonymous"><link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.12.1/css/v4-shims.min.css" integrity="sha256-wZjR52fzng1pJHwx4aV2AO3yyTOXrcDW7jBpJtTwVxw=" crossorigin="anonymous"><!-- bootstrap-toc --><script src="https://cdn.jsdelivr.net/gh/afeld/bootstrap-toc@v1.0.1/dist/bootstrap-toc.min.js" integrity="sha256-4veVQbu7//Lk5TSmc7YV48MxtMy98e26cf5MrgZYnwo=" crossorigin="anonymous"></script><!-- headroom.js --><script src="https://cdnjs.cloudflare.com/ajax/libs/headroom/0.11.0/headroom.min.js" integrity="sha256-AsUX4SJE1+yuDu5+mAVzJbuYNPHj/WroHuZ8Ir/CkE0=" crossorigin="anonymous"></script><script src="https://cdnjs.cloudflare.com/ajax/libs/headroom/0.11.0/jQuery.headroom.min.js" integrity="sha256-ZX/yNShbjqsohH1k95liqY9Gd8uOiE1S4vZc+9KQ1K4=" crossorigin="anonymous"></script><!-- clipboard.js --><script src="https://cdnjs.cloudflare.com/ajax/libs/clipboard.js/2.0.6/clipboard.min.js" integrity="sha256-inc5kl9MA1hkeYUt+EC3BhlIgyp/2jDIyBLS6k3UxPI=" crossorigin="anonymous"></script><!-- search --><script src="https://cdnjs.cloudflare.com/ajax/libs/fuse.js/6.4.6/fuse.js" integrity="sha512-zv6Ywkjyktsohkbp9bb45V6tEMoWhzFzXis+LrMehmJZZSys19Yxf1dopHx7WzIKxr5tK2dVcYmaCk2uqdjF4A==" crossorigin="anonymous"></script><script src="https://cdnjs.cloudflare.com/ajax/libs/autocomplete.js/0.38.0/autocomplete.jquery.min.js" integrity="sha512-GU9ayf+66Xx2TmpxqJpliWbT5PiGYxpaG8rfnBEk1LL8l1KGkRShhngwdXK1UgqhAzWpZHSiYPc09/NwDQIGyg==" crossorigin="anonymous"></script><script src="https://cdnjs.cloudflare.com/ajax/libs/mark.js/8.11.1/mark.min.js" integrity="sha512-5CYOlHXGh6QpOFA/TeTylKLWfB3ftPsde7AnmhuitiTX4K5SqCLBeKro6sPS8ilsz1Q4NRx3v8Ko2IBiszzdww==" crossorigin="anonymous"></script><!-- pkgdown --><script src="../pkgdown.js"></script><meta property="og:title" content="Covariance Matrix of Parameter Estimates -- Clark's methods — vcov.clark"><meta property="og:description" content="Function to compute the covariance matrix of the parameter estimates
for the ClarkLDF and ClarkCapeCod methods."><meta property="og:image" content="http://mages.github.io/ChainLadder/logo.png"><!-- mathjax --><script src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js" integrity="sha256-nvJJv9wWKEm88qvoQl9ekL2J+k/RWIsaSScxxlsrv8k=" crossorigin="anonymous"></script><script src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/config/TeX-AMS-MML_HTMLorMML.js" integrity="sha256-84DKXVJXs0/F8OTMzX4UR909+jtl4G7SPypPavF+GfA=" crossorigin="anonymous"></script><!--[if lt IE 9]>
<script src="https://oss.maxcdn.com/html5shiv/3.7.3/html5shiv.min.js"></script>
<script src="https://oss.maxcdn.com/respond/1.4.2/respond.min.js"></script>
<![endif]--></head><body>
    <a href="#main" class="visually-hidden-focusable">Skip to contents</a>
    

    <nav class="navbar fixed-top navbar-light navbar-expand-lg bg-light"><div class="container">
    
    <a class="navbar-brand me-2" href="../index.html">ChainLadder</a>

    <small class="nav-text text-muted me-auto" data-bs-toggle="tooltip" data-bs-placement="bottom" title="">0.2.18</small>

    
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
      <img src="../logo.png" class="logo" alt=""><h1>Covariance Matrix of Parameter Estimates -- Clark's methods</h1>
      
      <div class="d-none name"><code>vcov.clark.rd</code></div>
    </div>

    <div class="ref-description section level2">
    <p>Function to compute the covariance matrix of the parameter estimates
for the ClarkLDF and ClarkCapeCod methods.</p>
    </div>

    <div class="section level2">
    <h2 id="ref-usage">Usage<a class="anchor" aria-label="anchor" href="#ref-usage"></a></h2>
    <div class="sourceCode"><pre class="sourceCode r"><code><span><span class="co"># S3 method for clark</span></span>
<span><span class="fu"><a href="https://rdrr.io/r/stats/vcov.html" class="external-link">vcov</a></span><span class="op">(</span><span class="va">object</span>, <span class="va">...</span><span class="op">)</span></span></code></pre></div>
    </div>

    <div class="section level2">
    <h2 id="arguments">Arguments<a class="anchor" aria-label="anchor" href="#arguments"></a></h2>
    <dl><dt>object</dt>
<dd><p>object resulting from a run of the ClarkLDF or ClarkCapeCod functions.</p></dd>

<dt>...</dt>
<dd><p>not used.</p></dd>

</dl></div>
    <div class="section level2">
    <h2 id="details">Details<a class="anchor" aria-label="anchor" href="#details"></a></h2>
    <p>The covariance matrix of the estimated parameters is estimated
by the inverse of the Information matrix (see Clark, p. 53).
This function uses the "FI" and "sigma2" values returned by 
ClarkLDF and by ClarkCapeCod and calculates the matrix
<br>
-sigma2*FI^-1.</p>
    </div>
    <div class="section level2">
    <h2 id="references">References<a class="anchor" aria-label="anchor" href="#references"></a></h2>
    <p>Clark, David R., 
"LDF Curve-Fitting and Stochastic Reserving: A Maximum Likelihood Approach",
<em>Casualty Actuarial Society Forum</em>, Fall, 2003</p>
    </div>
    <div class="section level2">
    <h2 id="author">Author<a class="anchor" aria-label="anchor" href="#author"></a></h2>
    <p>Daniel Murphy</p>
    </div>
    <div class="section level2">
    <h2 id="see-also">See also<a class="anchor" aria-label="anchor" href="#see-also"></a></h2>
    <div class="dont-index"><p><code><a href="ClarkLDF.html">ClarkLDF</a></code>, <code><a href="ClarkCapeCod.html">ClarkCapeCod</a></code></p></div>
    </div>

    <div class="section level2">
    <h2 id="ref-examples">Examples<a class="anchor" aria-label="anchor" href="#ref-examples"></a></h2>
    <div class="sourceCode"><pre class="sourceCode r"><code><span class="r-in"><span></span></span>
<span class="r-in"><span><span class="va">x</span> <span class="op">&lt;-</span> <span class="va">GenIns</span></span></span>
<span class="r-in"><span><span class="fu"><a href="https://rdrr.io/r/base/colnames.html" class="external-link">colnames</a></span><span class="op">(</span><span class="va">x</span><span class="op">)</span> <span class="op">&lt;-</span> <span class="fl">12</span><span class="op">*</span><span class="fu"><a href="https://rdrr.io/r/base/numeric.html" class="external-link">as.numeric</a></span><span class="op">(</span><span class="fu"><a href="https://rdrr.io/r/base/colnames.html" class="external-link">colnames</a></span><span class="op">(</span><span class="va">x</span><span class="op">)</span><span class="op">)</span></span></span>
<span class="r-in"><span><span class="va">Y</span> <span class="op">&lt;-</span> <span class="fu"><a href="ClarkCapeCod.html">ClarkCapeCod</a></span><span class="op">(</span><span class="va">x</span>, Premium<span class="op">=</span><span class="fl">10000000</span><span class="op">+</span><span class="fl">400000</span><span class="op">*</span><span class="fl">0</span><span class="op">:</span><span class="fl">9</span>, maxage<span class="op">=</span><span class="fl">240</span><span class="op">)</span></span></span>
<span class="r-in"><span><span class="fu"><a href="https://rdrr.io/r/base/Round.html" class="external-link">round</a></span><span class="op">(</span><span class="fu"><a href="https://rdrr.io/r/stats/vcov.html" class="external-link">vcov</a></span><span class="op">(</span><span class="va">Y</span><span class="op">)</span>,<span class="fl">6</span><span class="op">)</span> <span class="co">## Compare to matrix on p. 69 of Clark's paper</span></span></span>
<span class="r-out co"><span class="r-pr">#&gt;</span>             ELR     omega     theta</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> ELR    0.002387 -0.002964  0.238498</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> omega -0.002964  0.007804 -0.396186</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> theta  0.238498 -0.396186 32.471686</span>
<span class="r-in"><span></span></span>
<span class="r-in"><span><span class="co"># The estimates of the loglogistic parameters</span></span></span>
<span class="r-in"><span><span class="va">Y</span><span class="op">$</span><span class="va">THETAG</span></span></span>
<span class="r-out co"><span class="r-pr">#&gt;</span>     omega     theta </span>
<span class="r-out co"><span class="r-pr">#&gt;</span>  1.448797 47.917507 </span>
<span class="r-in"><span><span class="co"># The standard errors of the estimated parameters</span></span></span>
<span class="r-in"><span><span class="fu"><a href="https://rdrr.io/r/base/MathFun.html" class="external-link">sqrt</a></span><span class="op">(</span><span class="fu"><a href="https://rdrr.io/r/utils/head.html" class="external-link">tail</a></span><span class="op">(</span><span class="fu"><a href="https://rdrr.io/r/base/diag.html" class="external-link">diag</a></span><span class="op">(</span><span class="fu"><a href="https://rdrr.io/r/stats/vcov.html" class="external-link">vcov</a></span><span class="op">(</span><span class="va">Y</span><span class="op">)</span><span class="op">)</span>, <span class="fl">2</span><span class="op">)</span><span class="op">)</span></span></span>
<span class="r-out co"><span class="r-pr">#&gt;</span>      omega      theta </span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 0.08834184 5.69839331 </span>
<span class="r-in"><span></span></span>
<span class="r-in"><span><span class="co"># The parameter risks of the estimated reserves are calculated </span></span></span>
<span class="r-in"><span><span class="co"># according to the formula on p. 54 of Clark's paper. For example, for</span></span></span>
<span class="r-in"><span><span class="co"># the 5th accident year, pre- and post-multiply the covariance matrix</span></span></span>
<span class="r-in"><span><span class="co"># by a matrix consisting of the gradient entries for just that accident year</span></span></span>
<span class="r-in"><span><span class="va">FVgrad5</span> <span class="op">&lt;-</span> <span class="fu"><a href="https://rdrr.io/r/base/matrix.html" class="external-link">matrix</a></span><span class="op">(</span><span class="va">Y</span><span class="op">$</span><span class="va">FutureValueGradient</span><span class="op">[</span>, <span class="fl">5</span><span class="op">]</span>, ncol<span class="op">=</span><span class="fl">1</span><span class="op">)</span></span></span>
<span class="r-in"><span><span class="fu"><a href="https://rdrr.io/r/base/MathFun.html" class="external-link">sqrt</a></span><span class="op">(</span><span class="fu"><a href="https://rdrr.io/r/base/t.html" class="external-link">t</a></span><span class="op">(</span><span class="va">FVgrad5</span><span class="op">)</span> <span class="op"><a href="https://rdrr.io/r/base/matmult.html" class="external-link">%*%</a></span> <span class="fu"><a href="https://rdrr.io/r/stats/vcov.html" class="external-link">vcov</a></span><span class="op">(</span><span class="va">Y</span><span class="op">)</span> <span class="op"><a href="https://rdrr.io/r/base/matmult.html" class="external-link">%*%</a></span> <span class="va">FVgrad5</span><span class="op">)</span> <span class="co">## compares to 314,829 in Clark's paper</span></span></span>
<span class="r-out co"><span class="r-pr">#&gt;</span>          [,1]</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> [1,] 312675.8</span>
<span class="r-in"><span></span></span>
<span class="r-in"><span><span class="co"># The estimated reserves for accident year 5:</span></span></span>
<span class="r-in"><span><span class="va">Y</span><span class="op">$</span><span class="va">FutureValue</span><span class="op">[</span><span class="fl">5</span><span class="op">]</span>   <span class="co">## compares to 2,046,646 in the paper</span></span></span>
<span class="r-out co"><span class="r-pr">#&gt;</span> [1] 2041248</span>
<span class="r-in"><span></span></span>
<span class="r-in"><span><span class="co"># Recalculate the parameter risk CV for all accident years in total (10.6% in paper):</span></span></span>
<span class="r-in"><span><span class="fu"><a href="https://rdrr.io/r/base/MathFun.html" class="external-link">sqrt</a></span><span class="op">(</span><span class="fu"><a href="https://rdrr.io/r/base/sum.html" class="external-link">sum</a></span><span class="op">(</span><span class="fu"><a href="https://rdrr.io/r/base/t.html" class="external-link">t</a></span><span class="op">(</span><span class="va">Y</span><span class="op">$</span><span class="va">FutureValueGradient</span><span class="op">)</span> <span class="op"><a href="https://rdrr.io/r/base/matmult.html" class="external-link">%*%</a></span> <span class="fu"><a href="https://rdrr.io/r/stats/vcov.html" class="external-link">vcov</a></span><span class="op">(</span><span class="va">Y</span><span class="op">)</span> <span class="op"><a href="https://rdrr.io/r/base/matmult.html" class="external-link">%*%</a></span> <span class="va">Y</span><span class="op">$</span><span class="va">FutureValueGradient</span><span class="op">)</span><span class="op">)</span> <span class="op">/</span> </span></span>
<span class="r-in"><span>    <span class="va">Y</span><span class="op">$</span><span class="va">Total</span><span class="op">$</span><span class="va">FutureValue</span></span></span>
<span class="r-out co"><span class="r-pr">#&gt;</span> [1] 0.1053735</span>
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

