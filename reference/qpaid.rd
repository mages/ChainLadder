<!DOCTYPE html>
<!-- Generated by pkgdown: do not edit by hand --><html lang="en"><head><meta http-equiv="Content-Type" content="text/html; charset=UTF-8"><meta charset="utf-8"><meta http-equiv="X-UA-Compatible" content="IE=edge"><meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"><meta name="description" content="Sample data to demonstrate how to work with triangles with a higher development period frequency than origin period frequency"><title>Quarterly run off triangle of accumulated claims data — qpaid • ChainLadder</title><!-- favicons --><link rel="icon" type="image/png" sizes="16x16" href="../favicon-16x16.png"><link rel="icon" type="image/png" sizes="32x32" href="../favicon-32x32.png"><link rel="apple-touch-icon" type="image/png" sizes="180x180" href="../apple-touch-icon.png"><link rel="apple-touch-icon" type="image/png" sizes="120x120" href="../apple-touch-icon-120x120.png"><link rel="apple-touch-icon" type="image/png" sizes="76x76" href="../apple-touch-icon-76x76.png"><link rel="apple-touch-icon" type="image/png" sizes="60x60" href="../apple-touch-icon-60x60.png"><script src="../deps/jquery-3.6.0/jquery-3.6.0.min.js"></script><meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"><link href="../deps/bootstrap-5.2.2/bootstrap.min.css" rel="stylesheet"><script src="../deps/bootstrap-5.2.2/bootstrap.bundle.min.js"></script><!-- Font Awesome icons --><link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.12.1/css/all.min.css" integrity="sha256-mmgLkCYLUQbXn0B1SRqzHar6dCnv9oZFPEC1g1cwlkk=" crossorigin="anonymous"><link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.12.1/css/v4-shims.min.css" integrity="sha256-wZjR52fzng1pJHwx4aV2AO3yyTOXrcDW7jBpJtTwVxw=" crossorigin="anonymous"><!-- bootstrap-toc --><script src="https://cdn.jsdelivr.net/gh/afeld/bootstrap-toc@v1.0.1/dist/bootstrap-toc.min.js" integrity="sha256-4veVQbu7//Lk5TSmc7YV48MxtMy98e26cf5MrgZYnwo=" crossorigin="anonymous"></script><!-- headroom.js --><script src="https://cdnjs.cloudflare.com/ajax/libs/headroom/0.11.0/headroom.min.js" integrity="sha256-AsUX4SJE1+yuDu5+mAVzJbuYNPHj/WroHuZ8Ir/CkE0=" crossorigin="anonymous"></script><script src="https://cdnjs.cloudflare.com/ajax/libs/headroom/0.11.0/jQuery.headroom.min.js" integrity="sha256-ZX/yNShbjqsohH1k95liqY9Gd8uOiE1S4vZc+9KQ1K4=" crossorigin="anonymous"></script><!-- clipboard.js --><script src="https://cdnjs.cloudflare.com/ajax/libs/clipboard.js/2.0.6/clipboard.min.js" integrity="sha256-inc5kl9MA1hkeYUt+EC3BhlIgyp/2jDIyBLS6k3UxPI=" crossorigin="anonymous"></script><!-- search --><script src="https://cdnjs.cloudflare.com/ajax/libs/fuse.js/6.4.6/fuse.js" integrity="sha512-zv6Ywkjyktsohkbp9bb45V6tEMoWhzFzXis+LrMehmJZZSys19Yxf1dopHx7WzIKxr5tK2dVcYmaCk2uqdjF4A==" crossorigin="anonymous"></script><script src="https://cdnjs.cloudflare.com/ajax/libs/autocomplete.js/0.38.0/autocomplete.jquery.min.js" integrity="sha512-GU9ayf+66Xx2TmpxqJpliWbT5PiGYxpaG8rfnBEk1LL8l1KGkRShhngwdXK1UgqhAzWpZHSiYPc09/NwDQIGyg==" crossorigin="anonymous"></script><script src="https://cdnjs.cloudflare.com/ajax/libs/mark.js/8.11.1/mark.min.js" integrity="sha512-5CYOlHXGh6QpOFA/TeTylKLWfB3ftPsde7AnmhuitiTX4K5SqCLBeKro6sPS8ilsz1Q4NRx3v8Ko2IBiszzdww==" crossorigin="anonymous"></script><!-- pkgdown --><script src="../pkgdown.js"></script><meta property="og:title" content="Quarterly run off triangle of accumulated claims data — qpaid"><meta property="og:description" content="Sample data to demonstrate how to work with triangles with a higher development period frequency than origin period frequency"><meta property="og:image" content="http://mages.github.io/ChainLadder/logo.png"><!-- mathjax --><script src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js" integrity="sha256-nvJJv9wWKEm88qvoQl9ekL2J+k/RWIsaSScxxlsrv8k=" crossorigin="anonymous"></script><script src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/config/TeX-AMS-MML_HTMLorMML.js" integrity="sha256-84DKXVJXs0/F8OTMzX4UR909+jtl4G7SPypPavF+GfA=" crossorigin="anonymous"></script><!--[if lt IE 9]>
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
      <img src="../logo.png" class="logo" alt=""><h1>Quarterly run off triangle of accumulated claims data</h1>
      
      <div class="d-none name"><code>qpaid.rd</code></div>
    </div>

    <div class="ref-description section level2">
    <p>Sample data to demonstrate how to work with triangles with a higher development period frequency than origin period frequency</p>
    </div>

    <div class="section level2">
    <h2 id="ref-usage">Usage<a class="anchor" aria-label="anchor" href="#ref-usage"></a></h2>
    <div class="sourceCode"><pre class="sourceCode r"><code><span><span class="fu"><a href="https://rdrr.io/r/utils/data.html" class="external-link">data</a></span><span class="op">(</span><span class="va">qpaid</span><span class="op">)</span>; <span class="fu"><a href="https://rdrr.io/r/utils/data.html" class="external-link">data</a></span><span class="op">(</span><span class="va">qincurred</span><span class="op">)</span></span></code></pre></div>
    </div>

    <div class="section level2">
    <h2 id="format">Format<a class="anchor" aria-label="anchor" href="#format"></a></h2>
    <p>A matrix with 12 accident years and 45 development quarters of claims costs.</p>
    </div>
    <div class="section level2">
    <h2 id="source">Source<a class="anchor" aria-label="anchor" href="#source"></a></h2>
    <p>Made up data for testing purpose</p>
    </div>

    <div class="section level2">
    <h2 id="ref-examples">Examples<a class="anchor" aria-label="anchor" href="#ref-examples"></a></h2>
    <div class="sourceCode"><pre class="sourceCode r"><code><span class="r-in"><span><span class="fu"><a href="https://rdrr.io/r/base/dim.html" class="external-link">dim</a></span><span class="op">(</span><span class="va">qpaid</span><span class="op">)</span></span></span>
<span class="r-out co"><span class="r-pr">#&gt;</span> [1] 12 45</span>
<span class="r-in"><span><span class="fu"><a href="https://rdrr.io/r/base/dim.html" class="external-link">dim</a></span><span class="op">(</span><span class="va">qincurred</span><span class="op">)</span></span></span>
<span class="r-out co"><span class="r-pr">#&gt;</span> [1] 12 45</span>
<span class="r-in"><span><span class="va">op</span><span class="op">=</span><span class="fu"><a href="https://rdrr.io/r/graphics/par.html" class="external-link">par</a></span><span class="op">(</span>mfrow<span class="op">=</span><span class="fu"><a href="https://rdrr.io/r/base/c.html" class="external-link">c</a></span><span class="op">(</span><span class="fl">1</span>,<span class="fl">2</span><span class="op">)</span><span class="op">)</span></span></span>
<span class="r-in"><span><span class="va">ymax</span> <span class="op">&lt;-</span> <span class="fu"><a href="https://rdrr.io/r/base/Extremes.html" class="external-link">max</a></span><span class="op">(</span><span class="fu"><a href="https://rdrr.io/r/base/c.html" class="external-link">c</a></span><span class="op">(</span><span class="va">qpaid</span>,<span class="va">qincurred</span><span class="op">)</span>,na.rm<span class="op">=</span><span class="cn">TRUE</span><span class="op">)</span><span class="op">*</span><span class="fl">1.05</span></span></span>
<span class="r-in"><span><span class="fu"><a href="https://rdrr.io/r/graphics/matplot.html" class="external-link">matplot</a></span><span class="op">(</span><span class="fu"><a href="https://rdrr.io/r/base/t.html" class="external-link">t</a></span><span class="op">(</span><span class="va">qpaid</span><span class="op">)</span>, type<span class="op">=</span><span class="st">"l"</span>, main<span class="op">=</span><span class="st">"Paid development"</span>, </span></span>
<span class="r-in"><span>      xlab<span class="op">=</span><span class="st">"Dev. quarter"</span>, ylab<span class="op">=</span><span class="st">"$"</span>, ylim<span class="op">=</span><span class="fu"><a href="https://rdrr.io/r/base/c.html" class="external-link">c</a></span><span class="op">(</span><span class="fl">0</span>,<span class="va">ymax</span><span class="op">)</span><span class="op">)</span></span></span>
<span class="r-in"><span><span class="fu"><a href="https://rdrr.io/r/graphics/matplot.html" class="external-link">matplot</a></span><span class="op">(</span><span class="fu"><a href="https://rdrr.io/r/base/t.html" class="external-link">t</a></span><span class="op">(</span><span class="va">qincurred</span><span class="op">)</span>, type<span class="op">=</span><span class="st">"l"</span>, main<span class="op">=</span><span class="st">"Incurred development"</span>, </span></span>
<span class="r-in"><span>          xlab<span class="op">=</span><span class="st">"Dev. quarter"</span>, ylab<span class="op">=</span><span class="st">"$"</span>, ylim<span class="op">=</span><span class="fu"><a href="https://rdrr.io/r/base/c.html" class="external-link">c</a></span><span class="op">(</span><span class="fl">0</span>,<span class="va">ymax</span><span class="op">)</span><span class="op">)</span></span></span>
<span class="r-plt img"><img src="qpaid-1.png" alt="" width="700" height="433"></span>
<span class="r-in"><span><span class="fu"><a href="https://rdrr.io/r/graphics/par.html" class="external-link">par</a></span><span class="op">(</span><span class="va">op</span><span class="op">)</span></span></span>
<span class="r-in"><span><span class="co">## MackChainLadder expects a quadratic matrix so let's expand </span></span></span>
<span class="r-in"><span><span class="co">## the triangle to a quarterly origin period.</span></span></span>
<span class="r-in"><span><span class="va">n</span> <span class="op">&lt;-</span> <span class="fu"><a href="https://rdrr.io/r/base/nrow.html" class="external-link">ncol</a></span><span class="op">(</span><span class="va">qpaid</span><span class="op">)</span></span></span>
<span class="r-in"><span><span class="va">Paid</span> <span class="op">&lt;-</span> <span class="fu"><a href="https://rdrr.io/r/base/matrix.html" class="external-link">matrix</a></span><span class="op">(</span><span class="cn">NA</span>, <span class="va">n</span>, <span class="va">n</span><span class="op">)</span></span></span>
<span class="r-in"><span><span class="va">Paid</span><span class="op">[</span><span class="fu"><a href="https://rdrr.io/r/base/seq.html" class="external-link">seq</a></span><span class="op">(</span><span class="fl">1</span>,<span class="va">n</span>,<span class="fl">4</span><span class="op">)</span>,<span class="op">]</span> <span class="op">&lt;-</span> <span class="va">qpaid</span></span></span>
<span class="r-in"><span><span class="va">M</span> <span class="op">&lt;-</span> <span class="fu"><a href="MackChainLadder.html">MackChainLadder</a></span><span class="op">(</span><span class="va">Paid</span><span class="op">)</span></span></span>
<span class="r-wrn co"><span class="r-pr">#&gt;</span> <span class="warning">Warning: </span>Information: essentially no variation in development data for period(s):</span>
<span class="r-wrn co"><span class="r-pr">#&gt;</span> '39-40', '40-41'</span>
<span class="r-in"><span><span class="fu"><a href="https://rdrr.io/r/graphics/plot.default.html" class="external-link">plot</a></span><span class="op">(</span><span class="va">M</span><span class="op">)</span></span></span>
<span class="r-plt img"><img src="qpaid-2.png" alt="" width="700" height="433"></span>
<span class="r-in"><span></span></span>
<span class="r-in"><span><span class="co"># We expand the incurred triangle in the same way </span></span></span>
<span class="r-in"><span><span class="va">Incurred</span> <span class="op">&lt;-</span> <span class="fu"><a href="https://rdrr.io/r/base/matrix.html" class="external-link">matrix</a></span><span class="op">(</span><span class="cn">NA</span>, <span class="va">n</span>, <span class="va">n</span><span class="op">)</span></span></span>
<span class="r-in"><span><span class="va">Incurred</span><span class="op">[</span><span class="fu"><a href="https://rdrr.io/r/base/seq.html" class="external-link">seq</a></span><span class="op">(</span><span class="fl">1</span>,<span class="va">n</span>,<span class="fl">4</span><span class="op">)</span>,<span class="op">]</span> <span class="op">&lt;-</span> <span class="va">qincurred</span></span></span>
<span class="r-in"><span></span></span>
<span class="r-in"><span><span class="co"># With the expanded triangles we can apply MunichChainLadder</span></span></span>
<span class="r-in"><span><span class="fu"><a href="MunichChainLadder.html">MunichChainLadder</a></span><span class="op">(</span><span class="va">Paid</span>, <span class="va">Incurred</span><span class="op">)</span></span></span>
<span class="r-wrn co"><span class="r-pr">#&gt;</span> <span class="warning">Warning: </span>Information: essentially no variation in development data for period(s):</span>
<span class="r-wrn co"><span class="r-pr">#&gt;</span> '39-40', '40-41'</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> MunichChainLadder(Paid = Paid, Incurred = Incurred)</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> </span>
<span class="r-out co"><span class="r-pr">#&gt;</span>    Latest Paid Latest Incurred Latest P/I Ratio Ult. Paid Ult. Incurred</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 1        1,100           1,100           1.0000     1,100         1,100</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 5        1,298           1,300           0.9985     1,300         1,300</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 9        1,198           1,200           0.9983     1,201         1,201</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 13       1,293           1,298           0.9961     1,301         1,301</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 17       1,573           1,583           0.9937     1,601         1,601</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 21       1,054           1,066           0.9887     1,100         1,100</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 25       1,387           1,411           0.9830     1,498         1,498</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 29       1,760           1,820           0.9670     1,995         1,995</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 33       1,100           1,221           0.9009     1,398         1,397</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 37         948           1,212           0.7822     1,590         1,590</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 41         183             422           0.4336     1,086         1,086</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 45           1              13           0.0769     1,073         1,073</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>    Ult. P/I Ratio</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 1               1</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 5               1</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 9               1</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 13              1</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 17              1</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 21              1</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 25              1</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 29              1</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 33              1</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 37              1</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 41              1</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 45              1</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> </span>
<span class="r-out co"><span class="r-pr">#&gt;</span> Totals</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>             Paid Incurred P/I Ratio</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> Latest:   12,895   13,646      0.94</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> Ultimate: 16,243   16,242      1.00</span>
<span class="r-in"><span></span></span>
<span class="r-in"><span><span class="co"># In the same way we can apply BootChainLadder</span></span></span>
<span class="r-in"><span><span class="co"># We reduce the size of bootstrap replicates R </span></span></span>
<span class="r-in"><span><span class="co"># from the default of 999 to 99 purely to reduce run time.</span></span></span>
<span class="r-in"><span><span class="fu"><a href="BootChainLadder.html">BootChainLadder</a></span><span class="op">(</span><span class="va">Paid</span>, R<span class="op">=</span><span class="fl">99</span><span class="op">)</span> </span></span>
<span class="r-out co"><span class="r-pr">#&gt;</span> BootChainLadder(Triangle = Paid, R = 99)</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> </span>
<span class="r-out co"><span class="r-pr">#&gt;</span>    Latest Mean Ultimate Mean IBNR IBNR.S.E IBNR 75% IBNR 95%</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 1   1,100         1,100      0.00     0.00     0.00     0.00</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 5   1,298         1,301      2.50     3.62     5.08     8.41</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 9   1,198         1,200      1.88     4.56     4.40     9.83</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 13  1,293         1,299      6.45     5.64     9.84    14.47</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 17  1,573         1,599     26.48    11.58    35.50    45.68</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 21  1,054         1,098     44.28    11.50    50.00    64.52</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 25  1,387         1,496    109.00    18.27   119.41   141.26</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 29  1,760         1,992    231.61    29.76   251.83   284.06</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 33  1,100         1,392    291.57    33.52   307.17   348.95</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 37    948         1,586    637.98    65.35   673.52   754.75</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 41    183         1,057    874.24   156.31   974.61 1,118.04</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 45      1         1,348  1,346.73 2,742.51 2,429.26 5,912.74</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> </span>
<span class="r-out co"><span class="r-pr">#&gt;</span>                 Totals</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> Latest:         12,895</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> Mean Ultimate:  16,468</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> Mean IBNR:       3,573</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> IBNR.S.E         2,716</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> Total IBNR 75%:  4,695</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> Total IBNR 95%:  8,124</span>
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

