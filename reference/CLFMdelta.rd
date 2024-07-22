<!DOCTYPE html>
<!-- Generated by pkgdown: do not edit by hand --><html lang="en"><head><meta http-equiv="Content-Type" content="text/html; charset=UTF-8"><meta charset="utf-8"><meta http-equiv="X-UA-Compatible" content="IE=edge"><meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"><title>Find "selection consistent" values of delta — CLFMdelta • ChainLadder</title><!-- favicons --><link rel="icon" type="image/png" sizes="16x16" href="../favicon-16x16.png"><link rel="icon" type="image/png" sizes="32x32" href="../favicon-32x32.png"><link rel="apple-touch-icon" type="image/png" sizes="180x180" href="../apple-touch-icon.png"><link rel="apple-touch-icon" type="image/png" sizes="120x120" href="../apple-touch-icon-120x120.png"><link rel="apple-touch-icon" type="image/png" sizes="76x76" href="../apple-touch-icon-76x76.png"><link rel="apple-touch-icon" type="image/png" sizes="60x60" href="../apple-touch-icon-60x60.png"><script src="../deps/jquery-3.6.0/jquery-3.6.0.min.js"></script><meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"><link href="../deps/bootstrap-5.3.1/bootstrap.min.css" rel="stylesheet"><script src="../deps/bootstrap-5.3.1/bootstrap.bundle.min.js"></script><link href="../deps/font-awesome-6.4.2/css/all.min.css" rel="stylesheet"><link href="../deps/font-awesome-6.4.2/css/v4-shims.min.css" rel="stylesheet"><script src="../deps/headroom-0.11.0/headroom.min.js"></script><script src="../deps/headroom-0.11.0/jQuery.headroom.min.js"></script><script src="../deps/bootstrap-toc-1.0.1/bootstrap-toc.min.js"></script><script src="../deps/clipboard.js-2.0.11/clipboard.min.js"></script><script src="../deps/search-1.0.0/autocomplete.jquery.min.js"></script><script src="../deps/search-1.0.0/fuse.min.js"></script><script src="../deps/search-1.0.0/mark.min.js"></script><!-- pkgdown --><script src="../pkgdown.js"></script><meta property="og:title" content="Find " selection consistent values of delta clfmdelta><meta name="description" content="This function finds the values of delta,
one for each development period,
such that the model coefficients resulting from
the 'chainladder' function with delta = solution are consistent
with an input vector of 'selected' development age-to-age factors."><meta property="og:description" content="This function finds the values of delta,
one for each development period,
such that the model coefficients resulting from
the 'chainladder' function with delta = solution are consistent
with an input vector of 'selected' development age-to-age factors."><meta property="og:image" content="http://mages.github.io/ChainLadder/logo.png"></head><body>
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
      <img src="../logo.png" class="logo" alt=""><h1>Find "selection consistent" values of delta</h1>

      <div class="d-none name"><code>CLFMdelta.rd</code></div>
    </div>

    <div class="ref-description section level2">
    <p>This function finds the values of delta,
one for each development period,
such that the model coefficients resulting from
the 'chainladder' function with delta = solution are consistent
with an input vector of 'selected' development age-to-age factors.</p>
    </div>

    <div class="section level2">
    <h2 id="ref-usage">Usage<a class="anchor" aria-label="anchor" href="#ref-usage"></a></h2>
    <div class="sourceCode"><pre><code>&lt;!-- %CLFMdelta(Triangle, selected, tolerance = .0005, step.a = 1, ...) --&gt;
CLFMdelta(Triangle, selected, tolerance = .0005, ...)</code></pre></div>
    </div>

    <div class="section level2">
    <h2 id="arguments">Arguments<a class="anchor" aria-label="anchor" href="#arguments"></a></h2>
    <p></p>
<dl><dt id="arg-triangle">Triangle<a class="anchor" aria-label="anchor" href="#arg-triangle"></a></dt>
<dd><p>cumulative claims triangle.  A (mxn)-matrix \(C_{ik}\)
    which is filled for \(k \leq n+1-i; i=1,\ldots,m; m\geq n \), see
    <code><a href="qpaid.html">qpaid</a></code> for how to use (mxn)-development triangles with
    m&lt;n, say higher development period frequency (e.g quarterly) than
    origin period frequency (e.g accident years).</p></dd>

  <dt id="arg-selected">selected<a class="anchor" aria-label="anchor" href="#arg-selected"></a></dt>
<dd><p>a vector of selected age-to-age factors or "link ratios",
    one for each development period of 'Triangle'</p></dd>

  <dt id="arg-tolerance">tolerance<a class="anchor" aria-label="anchor" href="#arg-tolerance"></a></dt>
<dd><p>a 'tolerance' parameters. Default: .0005;
    for each element of 'selected'
    a solution 'delta' will be found – if possible –
    so that the chainladder model indexed by
    'delta' results in a multiplicative coefficient within 'tolerance'
    of the 'selected' factor.</p></dd>

</dl><p><!-- %  \item{step.a}{the starting width of the search intervals} --></p>
<dl><dt id="arg--">...<a class="anchor" aria-label="anchor" href="#arg--"></a></dt>
<dd><p>not in use</p></dd>

  </dl></div>
    <div class="section level2">
    <h2 id="details">Details<a class="anchor" aria-label="anchor" href="#details"></a></h2>


<p>For a given input Triangle and vector of selected factors,
a search is conducted for chainladder models that are "consistent with" the
selected factors.
By "consistent with" is meant that the coefficients of the <code><a href="chainladder.html">chainladder</a></code>
function are equivalent to the selected factors.
Not all vectors of selected factors can be considered consistent with a given
Triangle;
feasibility is subject to restrictions on the 'selected' factors relative to
the input 'Triangle'.
See the References.</p>

<p>The default average produced by the <code>chainladder</code> function is the
volume weighted average and corresponds to <code>delta = 1</code> in the call
to that function;
<code>delta = 2</code> produces the simple average; and
<code>delta = 0</code> produces the "regression average", i.e.,
the slope of a regression line fit to the data
and running through the origin.
By convention, if the <code>selected</code> value corresponds to
the volume-weighted average, the simple average, or the regression average,
then the value returned will be 1, 2, and 0, respectively.</p>
<p>Other real-number values for <code>delta</code> will produce a different average.
The point of this function is to see if there exists a model as determined
by delta whose average is consistent with the value in the
<code>selected</code> vector.
That is not always possible. See the References.</p>
<p>It can be the case that one or more of the above three "standard averages"
will be quite close to each other
(indistinguishable within <code>tolerance</code>).
In that case, the value returned will be, in the following priority order
by convention,
1 (volume weighted average),
2 (simple average), or
0 (regression average).</p>
<p></p>
    </div>
    <div class="section level2">
    <h2 id="value">Value<a class="anchor" aria-label="anchor" href="#value"></a></h2>
    <p>A vector of real numbers, say delta0, such that
  <code>coef(chainladder(Triangle, delta = delta0))</code> = <code>selected</code>
  within <code>tolerance</code>.
  A <code>logical</code> attribute 'foundSolution' indicates if a solution was
  found for each element of <code>selected</code>.</p>
    </div>
    <div class="section level2">
    <h2 id="references">References<a class="anchor" aria-label="anchor" href="#references"></a></h2>


<p><cite>Bardis, Majidi, Murphy. A Family of Chain-Ladder Factor Models for Selected Link Ratios. <em>Variance</em>. Pending. Variance 6:2, 2012, pp. 143-160.</cite></p>
    </div>
    <div class="section level2">
    <h2 id="author">Author<a class="anchor" aria-label="anchor" href="#author"></a></h2>
    <p>Dan Murphy</p>
    </div>

    <div class="section level2">
    <h2 id="ref-examples">Examples<a class="anchor" aria-label="anchor" href="#ref-examples"></a></h2>
    <div class="sourceCode"><pre class="sourceCode r"><code><span class="r-in"><span></span></span>
<span class="r-in"><span><span class="va">x</span> <span class="op">&lt;-</span> <span class="va">RAA</span><span class="op">[</span><span class="fl">1</span><span class="op">:</span><span class="fl">9</span>,<span class="fl">1</span><span class="op">]</span></span></span>
<span class="r-in"><span><span class="va">y</span> <span class="op">&lt;-</span> <span class="va">RAA</span><span class="op">[</span><span class="fl">1</span><span class="op">:</span><span class="fl">9</span>,<span class="fl">2</span><span class="op">]</span></span></span>
<span class="r-in"><span><span class="cn">F</span> <span class="op">&lt;-</span> <span class="va">y</span><span class="op">/</span><span class="va">x</span></span></span>
<span class="r-in"><span><span class="fu">CLFMdelta</span><span class="op">(</span><span class="va">RAA</span><span class="op">[</span><span class="fl">1</span><span class="op">:</span><span class="fl">9</span>, <span class="fl">1</span><span class="op">:</span><span class="fl">2</span><span class="op">]</span>, selected <span class="op">=</span> <span class="fu"><a href="https://rdrr.io/r/base/mean.html" class="external-link">mean</a></span><span class="op">(</span><span class="cn">F</span><span class="op">)</span><span class="op">)</span> <span class="co"># value is 2, 'foundSolution' is TRUE</span></span></span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 1 </span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 2 </span>
<span class="r-out co"><span class="r-pr">#&gt;</span> attr(,"foundSolution")</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> [1] TRUE</span>
<span class="r-in"><span><span class="fu">CLFMdelta</span><span class="op">(</span><span class="va">RAA</span><span class="op">[</span><span class="fl">1</span><span class="op">:</span><span class="fl">9</span>, <span class="fl">1</span><span class="op">:</span><span class="fl">2</span><span class="op">]</span>, selected <span class="op">=</span> <span class="fu"><a href="https://rdrr.io/r/base/sum.html" class="external-link">sum</a></span><span class="op">(</span><span class="va">y</span><span class="op">)</span> <span class="op">/</span> <span class="fu"><a href="https://rdrr.io/r/base/sum.html" class="external-link">sum</a></span><span class="op">(</span><span class="va">x</span><span class="op">)</span><span class="op">)</span> <span class="co"># value is 1, 'foundSolution' is TRUE</span></span></span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 1 </span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 1 </span>
<span class="r-out co"><span class="r-pr">#&gt;</span> attr(,"foundSolution")</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> [1] TRUE</span>
<span class="r-in"><span></span></span>
<span class="r-in"><span><span class="va">selected</span> <span class="op">&lt;-</span> <span class="fu"><a href="https://rdrr.io/r/base/mean.html" class="external-link">mean</a></span><span class="op">(</span><span class="fu"><a href="https://rdrr.io/r/base/c.html" class="external-link">c</a></span><span class="op">(</span><span class="fu"><a href="https://rdrr.io/r/base/mean.html" class="external-link">mean</a></span><span class="op">(</span><span class="cn">F</span><span class="op">)</span>, <span class="fu"><a href="https://rdrr.io/r/base/sum.html" class="external-link">sum</a></span><span class="op">(</span><span class="va">y</span><span class="op">)</span> <span class="op">/</span> <span class="fu"><a href="https://rdrr.io/r/base/sum.html" class="external-link">sum</a></span><span class="op">(</span><span class="va">x</span><span class="op">)</span><span class="op">)</span><span class="op">)</span> <span class="co"># an average of averages</span></span></span>
<span class="r-in"><span><span class="fu">CLFMdelta</span><span class="op">(</span><span class="va">RAA</span><span class="op">[</span><span class="fl">1</span><span class="op">:</span><span class="fl">9</span>, <span class="fl">1</span><span class="op">:</span><span class="fl">2</span><span class="op">]</span>, <span class="va">selected</span><span class="op">)</span> <span class="co"># about 1.725</span></span></span>
<span class="r-out co"><span class="r-pr">#&gt;</span>        1 </span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 1.724596 </span>
<span class="r-out co"><span class="r-pr">#&gt;</span> attr(,"foundSolution")</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> [1] TRUE</span>
<span class="r-in"><span><span class="fu">CLFMdelta</span><span class="op">(</span><span class="va">RAA</span><span class="op">[</span><span class="fl">1</span><span class="op">:</span><span class="fl">9</span>, <span class="fl">1</span><span class="op">:</span><span class="fl">2</span><span class="op">]</span>, selected <span class="op">=</span> <span class="fl">2</span><span class="op">)</span> <span class="co"># negative solutions are possible</span></span></span>
<span class="r-out co"><span class="r-pr">#&gt;</span>         1 </span>
<span class="r-out co"><span class="r-pr">#&gt;</span> -1.160568 </span>
<span class="r-out co"><span class="r-pr">#&gt;</span> attr(,"foundSolution")</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> [1] TRUE</span>
<span class="r-in"><span></span></span>
<span class="r-in"><span><span class="co"># Demonstrating an "unreasonable" selected factor.</span></span></span>
<span class="r-in"><span><span class="fu">CLFMdelta</span><span class="op">(</span><span class="va">RAA</span><span class="op">[</span><span class="fl">1</span><span class="op">:</span><span class="fl">9</span>, <span class="fl">1</span><span class="op">:</span><span class="fl">2</span><span class="op">]</span>, selected <span class="op">=</span> <span class="fl">1.9</span><span class="op">)</span> <span class="co"># NA solution, with warning</span></span></span>
<span class="r-wrn co"><span class="r-pr">#&gt;</span> <span class="warning">Warning: </span>No optimal delta solution for age 1. Returning NA.</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>  1 </span>
<span class="r-out co"><span class="r-pr">#&gt;</span> NA </span>
<span class="r-out co"><span class="r-pr">#&gt;</span> attr(,"foundSolution")</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> [1] FALSE</span>
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
