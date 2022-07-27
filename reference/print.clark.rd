<!DOCTYPE html>
<!-- Generated by pkgdown: do not edit by hand --><html lang="en"><head><meta http-equiv="Content-Type" content="text/html; charset=UTF-8"><meta charset="utf-8"><meta http-equiv="X-UA-Compatible" content="IE=edge"><meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"><meta name="description" content="Functions to print the results
of the ClarkLDF and ClarkCapeCod methods."><title>Print results of Clark methods — print.clark • ChainLadder</title><!-- favicons --><link rel="icon" type="image/png" sizes="16x16" href="../favicon-16x16.png"><link rel="icon" type="image/png" sizes="32x32" href="../favicon-32x32.png"><link rel="apple-touch-icon" type="image/png" sizes="180x180" href="../apple-touch-icon.png"><link rel="apple-touch-icon" type="image/png" sizes="120x120" href="../apple-touch-icon-120x120.png"><link rel="apple-touch-icon" type="image/png" sizes="76x76" href="../apple-touch-icon-76x76.png"><link rel="apple-touch-icon" type="image/png" sizes="60x60" href="../apple-touch-icon-60x60.png"><script src="../deps/jquery-3.6.0/jquery-3.6.0.min.js"></script><meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"><link href="../deps/bootstrap-5.1.3/bootstrap.min.css" rel="stylesheet"><script src="../deps/bootstrap-5.1.3/bootstrap.bundle.min.js"></script><!-- Font Awesome icons --><link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.12.1/css/all.min.css" integrity="sha256-mmgLkCYLUQbXn0B1SRqzHar6dCnv9oZFPEC1g1cwlkk=" crossorigin="anonymous"><link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.12.1/css/v4-shims.min.css" integrity="sha256-wZjR52fzng1pJHwx4aV2AO3yyTOXrcDW7jBpJtTwVxw=" crossorigin="anonymous"><!-- bootstrap-toc --><script src="https://cdn.rawgit.com/afeld/bootstrap-toc/v1.0.1/dist/bootstrap-toc.min.js"></script><!-- headroom.js --><script src="https://cdnjs.cloudflare.com/ajax/libs/headroom/0.11.0/headroom.min.js" integrity="sha256-AsUX4SJE1+yuDu5+mAVzJbuYNPHj/WroHuZ8Ir/CkE0=" crossorigin="anonymous"></script><script src="https://cdnjs.cloudflare.com/ajax/libs/headroom/0.11.0/jQuery.headroom.min.js" integrity="sha256-ZX/yNShbjqsohH1k95liqY9Gd8uOiE1S4vZc+9KQ1K4=" crossorigin="anonymous"></script><!-- clipboard.js --><script src="https://cdnjs.cloudflare.com/ajax/libs/clipboard.js/2.0.6/clipboard.min.js" integrity="sha256-inc5kl9MA1hkeYUt+EC3BhlIgyp/2jDIyBLS6k3UxPI=" crossorigin="anonymous"></script><!-- search --><script src="https://cdnjs.cloudflare.com/ajax/libs/fuse.js/6.4.6/fuse.js" integrity="sha512-zv6Ywkjyktsohkbp9bb45V6tEMoWhzFzXis+LrMehmJZZSys19Yxf1dopHx7WzIKxr5tK2dVcYmaCk2uqdjF4A==" crossorigin="anonymous"></script><script src="https://cdnjs.cloudflare.com/ajax/libs/autocomplete.js/0.38.0/autocomplete.jquery.min.js" integrity="sha512-GU9ayf+66Xx2TmpxqJpliWbT5PiGYxpaG8rfnBEk1LL8l1KGkRShhngwdXK1UgqhAzWpZHSiYPc09/NwDQIGyg==" crossorigin="anonymous"></script><script src="https://cdnjs.cloudflare.com/ajax/libs/mark.js/8.11.1/mark.min.js" integrity="sha512-5CYOlHXGh6QpOFA/TeTylKLWfB3ftPsde7AnmhuitiTX4K5SqCLBeKro6sPS8ilsz1Q4NRx3v8Ko2IBiszzdww==" crossorigin="anonymous"></script><!-- pkgdown --><script src="../pkgdown.js"></script><meta property="og:title" content="Print results of Clark methods — print.clark"><meta property="og:description" content="Functions to print the results
of the ClarkLDF and ClarkCapeCod methods."><meta property="og:image" content="http://mages.github.io/ChainLadder/logo.png"><!-- mathjax --><script src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js" integrity="sha256-nvJJv9wWKEm88qvoQl9ekL2J+k/RWIsaSScxxlsrv8k=" crossorigin="anonymous"></script><script src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/config/TeX-AMS-MML_HTMLorMML.js" integrity="sha256-84DKXVJXs0/F8OTMzX4UR909+jtl4G7SPypPavF+GfA=" crossorigin="anonymous"></script><!--[if lt IE 9]>
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
      <img src="../logo.png" class="logo" alt=""><h1>Print results of Clark methods</h1>
      
      <div class="d-none name"><code>print.clark.rd</code></div>
    </div>

    <div class="ref-description section level2">
    <p>Functions to print the results
of the ClarkLDF and ClarkCapeCod methods.</p>
    </div>

    <div class="section level2">
    <h2 id="ref-usage">Usage<a class="anchor" aria-label="anchor" href="#ref-usage"></a></h2>
    <div class="sourceCode"><pre><code>&lt;!-- %- \method{print}{clark}(x, \dots) --&gt;
# S3 method for ClarkLDF
print(x, Amountdigits=0, LDFdigits=3, CVdigits=3, 
            row.names = FALSE, ...)

# S3 method for ClarkCapeCod
print(x, Amountdigits=0, ELRdigits=3, Gdigits=4, CVdigits=3,
            row.names = FALSE, ...)</code></pre></div>
    </div>

    <div class="section level2">
    <h2 id="arguments">Arguments<a class="anchor" aria-label="anchor" href="#arguments"></a></h2>
    <dl><dt>x</dt>
<dd><p>object resulting from a run of the ClarkLDF or ClarkCapeCod function.</p></dd>

    <dt>Amountdigits</dt>
<dd><p>number of digits to display to the right of the decimal point for "amount" columns</p></dd>

    <dt>LDFdigits</dt>
<dd><p>number of digits to display to the right of the decimal point for the
        loss development factor (LDF) column</p></dd>

    <dt>CVdigits</dt>
<dd><p>number of digits to display to the right of the decimal point for the 
        coefficient of variation (CV) column</p></dd>

    <dt>ELRdigits</dt>
<dd><p>number of digits to display to the right of the decimal point for the
        expected loss ratio (ELR) column</p></dd>

    <dt>Gdigits</dt>
<dd><p>number of digits to display to the right of the decimal point for the
        "growth function factor" column; 
        default of 4 conforms with the table on pp. 67, 68 of Clark's paper</p></dd>

    <dt>row.names</dt>
<dd><p>logical (or character vector), 
        indicating whether (or what) row names should be printed
        (same as for <code><a href="https://rdrr.io/r/base/print.dataframe.html" class="external-link">print.data.frame</a></code>)</p></dd>

    <dt>...</dt>
<dd><p>further arguments passed to <code>print</code></p></dd>

</dl></div>
    <div class="section level2">
    <h2 id="details">Details<a class="anchor" aria-label="anchor" href="#details"></a></h2>
    <p>Display the default information in "pretty format" resulting from 
a run of the "LDF Method" or "Cape Cod Method" --
a "Development-type" exhibit for Clark's "LDF Method," 
a "Bornhuetter-Ferguson-type" exhibit for Clark's "Cape Cod Method."</p>
<p>As usual, typing the name of such an object at the console
invokes its <code>print</code> method.</p>
    </div>
    <div class="section level2">
    <h2 id="value">Value<a class="anchor" aria-label="anchor" href="#value"></a></h2>
    

<p><code>data.frame</code>s whose columns are the <code>character</code> representation
of their respective <code><a href="summary.clark.html">summary.ClarkLDF</a></code></p>
 

<p>or <code><a href="summary.clark.html">summary.ClarkCapeCod</a></code></p>
<p></p>
<p><code>data.frame</code>s.</p>
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
    <div class="dont-index"><p><code><a href="summary.clark.html">summary.ClarkLDF</a></code> and <code><a href="summary.clark.html">summary.ClarkCapeCod</a></code></p></div>
    </div>

    <div class="section level2">
    <h2 id="ref-examples">Examples<a class="anchor" aria-label="anchor" href="#ref-examples"></a></h2>
    <div class="sourceCode"><pre class="sourceCode r"><code><span class="r-in"><span><span class="va">X</span> <span class="op">&lt;-</span> <span class="va">GenIns</span></span></span>
<span class="r-in"><span><span class="fu"><a href="https://rdrr.io/r/base/colnames.html" class="external-link">colnames</a></span><span class="op">(</span><span class="va">X</span><span class="op">)</span> <span class="op">&lt;-</span> <span class="fl">12</span><span class="op">*</span><span class="fu"><a href="https://rdrr.io/r/base/numeric.html" class="external-link">as.numeric</a></span><span class="op">(</span><span class="fu"><a href="https://rdrr.io/r/base/colnames.html" class="external-link">colnames</a></span><span class="op">(</span><span class="va">X</span><span class="op">)</span><span class="op">)</span></span></span>
<span class="r-in"><span><span class="va">y</span> <span class="op">&lt;-</span> <span class="fu"><a href="ClarkCapeCod.html">ClarkCapeCod</a></span><span class="op">(</span><span class="va">X</span>, Premium<span class="op">=</span><span class="fl">10000000</span><span class="op">+</span><span class="fl">400000</span><span class="op">*</span><span class="fl">0</span><span class="op">:</span><span class="fl">9</span>, maxage<span class="op">=</span><span class="fl">240</span><span class="op">)</span></span></span>
<span class="r-in"><span><span class="fu"><a href="https://rdrr.io/r/base/summary.html" class="external-link">summary</a></span><span class="op">(</span><span class="va">y</span><span class="op">)</span> </span></span>
<span class="r-out co"><span class="r-pr">#&gt;</span>       Origin CurrentValue  Premium      ELR FutureGrowthFactor FutureValue</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 1          1      3901463 1.00e+07 0.597026          0.1303902    778463.4</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 2          2      5339085 1.04e+07 0.597026          0.1594364    989952.1</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 3          3      4909315 1.08e+07 0.597026          0.1950165   1257443.3</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 4          4      4588268 1.12e+07 0.597026          0.2391815   1599332.9</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 5          5      3873311 1.16e+07 0.597026          0.2947437   2041248.2</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 6          6      3691712 1.20e+07 0.597026          0.3654987   2618546.9</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 7          7      3483130 1.24e+07 0.597026          0.4562722   3377839.6</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 8          8      2864498 1.28e+07 0.597026          0.5720671   4371698.3</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 9          9      1363294 1.32e+07 0.597026          0.7137828   5625139.6</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 10        10       344014 1.36e+07 0.597026          0.8617075   6996681.1</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> Total  Total     34358090 1.18e+08       NA                 NA  29656345.3</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>       UltimateValue  StdError        CV</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 1           4679926  269248.7 0.3458721</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 2           6329037  311328.7 0.3144887</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 3           6166758  358975.2 0.2854803</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 4           6187601  412520.2 0.2579326</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 5           5914559  471780.8 0.2311237</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 6           6310259  535626.0 0.2045509</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 7           6860970  601534.4 0.1780826</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 8           7236196  665860.7 0.1523117</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 9           6988434  726347.9 0.1291253</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 10          7340695  786672.0 0.1124350</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> Total      64014435 3402778.6 0.1147403</span>
<span class="r-in"><span><span class="fu"><a href="https://rdrr.io/r/base/print.html" class="external-link">print</a></span><span class="op">(</span><span class="va">y</span><span class="op">)</span>  <span class="co"># (or simply 'y') Same as summary(y) but with "pretty formats"</span></span></span>
<span class="r-out co"><span class="r-pr">#&gt;</span>  Origin CurrentValue     Premium   ELR FutureGrowthFactor FutureValue</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>       1    3,901,463  10,000,000 0.597             0.1304     778,463</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>       2    5,339,085  10,400,000 0.597             0.1594     989,952</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>       3    4,909,315  10,800,000 0.597             0.1950   1,257,443</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>       4    4,588,268  11,200,000 0.597             0.2392   1,599,333</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>       5    3,873,311  11,600,000 0.597             0.2947   2,041,248</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>       6    3,691,712  12,000,000 0.597             0.3655   2,618,547</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>       7    3,483,130  12,400,000 0.597             0.4563   3,377,840</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>       8    2,864,498  12,800,000 0.597             0.5721   4,371,698</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>       9    1,363,294  13,200,000 0.597             0.7138   5,625,140</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>      10      344,014  13,600,000 0.597             0.8617   6,996,681</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>   Total   34,358,090 118,000,000                           29,656,345</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>  UltimateValue  StdError  CV%</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>      4,679,926   269,249 34.6</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>      6,329,037   311,329 31.4</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>      6,166,758   358,975 28.5</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>      6,187,601   412,520 25.8</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>      5,914,559   471,781 23.1</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>      6,310,259   535,626 20.5</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>      6,860,970   601,534 17.8</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>      7,236,196   665,861 15.2</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>      6,988,434   726,348 12.9</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>      7,340,695   786,672 11.2</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>     64,014,435 3,402,779 11.5</span>
<span class="r-in"><span></span></span>
<span class="r-in"><span><span class="co">## Greater growth factors when projecting to infinite maximum age</span></span></span>
<span class="r-in"><span><span class="fu"><a href="ClarkCapeCod.html">ClarkCapeCod</a></span><span class="op">(</span><span class="va">X</span>, Premium<span class="op">=</span><span class="fl">10000000</span><span class="op">+</span><span class="fl">400000</span><span class="op">*</span><span class="fl">0</span><span class="op">:</span><span class="fl">9</span>, maxage<span class="op">=</span><span class="cn">Inf</span><span class="op">)</span></span></span>
<span class="r-out co"><span class="r-pr">#&gt;</span>  Origin CurrentValue     Premium   ELR FutureGrowthFactor FutureValue</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>       1    3,901,463  10,000,000 0.597             0.2217   1,323,698</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>       2    5,339,085  10,400,000 0.597             0.2508   1,556,996</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>       3    4,909,315  10,800,000 0.597             0.2863   1,846,297</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>       4    4,588,268  11,200,000 0.597             0.3305   2,209,996</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>       5    3,873,311  11,600,000 0.597             0.3861   2,673,721</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>       6    3,691,712  12,000,000 0.597             0.4568   3,272,829</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>       7    3,483,130  12,400,000 0.597             0.5476   4,053,931</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>       8    2,864,498  12,800,000 0.597             0.6634   5,069,599</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>       9    1,363,294  13,200,000 0.597             0.8051   6,344,850</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>      10      344,014  13,600,000 0.597             0.9530   7,738,201</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>   Total   34,358,090 118,000,000                           36,090,118</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>  UltimateValue  StdError  CV%</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>      5,225,161   443,978 33.5</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>      6,896,081   489,929 31.5</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>      6,755,612   541,157 29.3</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>      6,798,264   597,696 27.0</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>      6,547,032   658,893 24.6</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>      6,964,541   722,850 22.1</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>      7,537,061   785,803 19.4</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>      7,934,097   842,378 16.6</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>      7,708,144   889,806 14.0</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>      8,082,215   941,203 12.2</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>     70,448,208 5,378,616 14.9</span>
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

