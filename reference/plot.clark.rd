<!DOCTYPE html>
<!-- Generated by pkgdown: do not edit by hand --><html lang="en"><head><meta http-equiv="Content-Type" content="text/html; charset=UTF-8"><meta charset="utf-8"><meta http-equiv="X-UA-Compatible" content="IE=edge"><meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"><title>Plot Clark method residuals — plot.clark • ChainLadder</title><!-- favicons --><link rel="icon" type="image/png" sizes="16x16" href="../favicon-16x16.png"><link rel="icon" type="image/png" sizes="32x32" href="../favicon-32x32.png"><link rel="apple-touch-icon" type="image/png" sizes="180x180" href="../apple-touch-icon.png"><link rel="apple-touch-icon" type="image/png" sizes="120x120" href="../apple-touch-icon-120x120.png"><link rel="apple-touch-icon" type="image/png" sizes="76x76" href="../apple-touch-icon-76x76.png"><link rel="apple-touch-icon" type="image/png" sizes="60x60" href="../apple-touch-icon-60x60.png"><script src="../deps/jquery-3.6.0/jquery-3.6.0.min.js"></script><meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"><link href="../deps/bootstrap-5.3.1/bootstrap.min.css" rel="stylesheet"><script src="../deps/bootstrap-5.3.1/bootstrap.bundle.min.js"></script><link href="../deps/font-awesome-6.4.2/css/all.min.css" rel="stylesheet"><link href="../deps/font-awesome-6.4.2/css/v4-shims.min.css" rel="stylesheet"><script src="../deps/headroom-0.11.0/headroom.min.js"></script><script src="../deps/headroom-0.11.0/jQuery.headroom.min.js"></script><script src="../deps/bootstrap-toc-1.0.1/bootstrap-toc.min.js"></script><script src="../deps/clipboard.js-2.0.11/clipboard.min.js"></script><script src="../deps/search-1.0.0/autocomplete.jquery.min.js"></script><script src="../deps/search-1.0.0/fuse.min.js"></script><script src="../deps/search-1.0.0/mark.min.js"></script><!-- pkgdown --><script src="../pkgdown.js"></script><meta property="og:title" content="Plot Clark method residuals — plot.clark"><meta name="description" content="Function to plot the residuals of the Clark LDF and Cape Cod methods."><meta property="og:description" content="Function to plot the residuals of the Clark LDF and Cape Cod methods."><meta property="og:image" content="http://mages.github.io/ChainLadder/logo.png"></head><body>
    <a href="#main" class="visually-hidden-focusable">Skip to contents</a>


    <nav class="navbar navbar-expand-lg fixed-top bg-light" data-bs-theme="light" aria-label="Site navigation"><div class="container">

    <a class="navbar-brand me-2" href="../index.html">ChainLadder</a>

    <small class="nav-text text-muted me-auto" data-bs-toggle="tooltip" data-bs-placement="bottom" title="">0.2.20</small>


    <button class="navbar-toggler" type="button" data-bs-toggle="collapse" data-bs-target="#navbar" aria-controls="navbar" aria-expanded="false" aria-label="Toggle navigation">
      <span class="navbar-toggler-icon"></span>
    </button>

    <div id="navbar" class="collapse navbar-collapse ms-3">
      <ul class="navbar-nav me-auto"><li class="nav-item"><a class="nav-link" href="../articles/ChainLadder.html">Get started</a></li>
<li class="active nav-item"><a class="nav-link" href="../reference/index.html">Reference</a></li>
<li class="nav-item"><a class="nav-link" href="../news/index.html">Changelog</a></li>
      </ul><ul class="navbar-nav"><li class="nav-item"><form class="form-inline" role="search">
 <input class="form-control" type="search" name="search-input" id="search-input" autocomplete="off" aria-label="Search site" placeholder="Search for" data-search-index="../search.json"></form></li>
<li class="nav-item"><a class="external-link nav-link" href="https://github.com/mages/ChainLadder/" aria-label="GitHub"><span class="fa fab fa-github fa-lg"></span></a></li>
      </ul></div>


  </div>
</nav><div class="container template-reference-topic">
<div class="row">
  <main id="main" class="col-md-9"><div class="page-header">
      <img src="../logo.png" class="logo" alt=""><h1>Plot Clark method residuals</h1>

      <div class="d-none name"><code>plot.clark.rd</code></div>
    </div>

    <div class="ref-description section level2">
    <p>Function to plot the residuals of the Clark LDF and Cape Cod methods.</p>
    </div>

    <div class="section level2">
    <h2 id="ref-usage">Usage<a class="anchor" aria-label="anchor" href="#ref-usage"></a></h2>
    <div class="sourceCode"><pre class="sourceCode r"><code><span><span class="co"># S3 method for class 'clark'</span></span>
<span><span class="fu"><a href="https://rdrr.io/r/graphics/plot.default.html" class="external-link">plot</a></span><span class="op">(</span><span class="va">x</span>, <span class="va">...</span><span class="op">)</span></span></code></pre></div>
    </div>

    <div class="section level2">
    <h2 id="arguments">Arguments<a class="anchor" aria-label="anchor" href="#arguments"></a></h2>


<dl><dt id="arg-x">x<a class="anchor" aria-label="anchor" href="#arg-x"></a></dt>
<dd><p>object resulting from a run of the ClarkLDF or ClarkCapeCod functions.</p></dd>

<dt id="arg--">...<a class="anchor" aria-label="anchor" href="#arg--"></a></dt>
<dd><p>not used.</p></dd>

</dl></div>
    <div class="section level2">
    <h2 id="details">Details<a class="anchor" aria-label="anchor" href="#details"></a></h2>
    <p>If Clark's model is appropriate for the actual data,
then the standardized residuals should appear as
independent standard normal random variables.
This function creates four plots of standardized residuals on a single page:</p><ol><li><p>By origin</p></li>
<li><p>By age</p></li>
<li><p>By fitted value</p></li>
<li><p>Normal Q-Q plot with results of Shapiro-Wilk test</p></li>
</ol><p>If the model is appropriate then there should not appear to be any trend in the
standardized residuals or any systematic differences in the spread
about the line y = 0.
The Shapiro-Wilk p-value shown in the fourth plot gives an indication
of how closely the standardized residuals can be considered "draws"
from a standard normal random variable.</p>
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
<span class="r-in"><span><span class="va">X</span> <span class="op">&lt;-</span> <span class="va">GenIns</span></span></span>
<span class="r-in"><span><span class="va">Y</span> <span class="op">&lt;-</span> <span class="fu"><a href="ClarkLDF.html">ClarkLDF</a></span><span class="op">(</span><span class="va">GenIns</span>, maxage<span class="op">=</span><span class="cn">Inf</span>, G<span class="op">=</span><span class="st">"weibull"</span><span class="op">)</span></span></span>
<span class="r-in"><span><span class="fu"><a href="https://rdrr.io/r/graphics/plot.default.html" class="external-link">plot</a></span><span class="op">(</span><span class="va">Y</span><span class="op">)</span>  <span class="co"># One obvious outlier, shapiro test flunked</span></span></span>
<span class="r-in"><span><span class="va">X</span><span class="op">[</span><span class="fl">4</span>,<span class="fl">4</span><span class="op">]</span> <span class="op">&lt;-</span> <span class="cn">NA</span>  <span class="co"># remove the outlier</span></span></span>
<span class="r-in"><span><span class="va">Z</span> <span class="op">&lt;-</span> <span class="fu"><a href="ClarkLDF.html">ClarkLDF</a></span><span class="op">(</span><span class="va">GenIns</span>, maxage<span class="op">=</span><span class="cn">Inf</span>, G<span class="op">=</span><span class="st">"weibull"</span><span class="op">)</span></span></span>
<span class="r-in"><span><span class="fu"><a href="https://rdrr.io/r/graphics/plot.default.html" class="external-link">plot</a></span><span class="op">(</span><span class="va">Z</span><span class="op">)</span>  <span class="co"># Q-Q plot looks good</span></span></span>
<span class="r-plt img"><img src="plot.clark-1.png" alt="" width="700" height="433"></span>
<span class="r-in"><span></span></span>
</code></pre></div>
    </div>
  </main><aside class="col-md-3"><nav id="toc" aria-label="Table of contents"><h2>On this page</h2>
    </nav></aside></div>


    <footer><div class="pkgdown-footer-left">
  <p>Developed by Markus Gesmann, Daniel Murphy, Yanwei (Wayne) Zhang, Alessandro Carrato, Mario Wuthrich, Fabio Concina, Eric Dal Moro.</p>
</div>

<div class="pkgdown-footer-right">
  <p>Site built with <a href="https://pkgdown.r-lib.org/" class="external-link">pkgdown</a> 2.1.0.</p>
</div>

    </footer></div>





  </body></html>
