<!DOCTYPE html>
<!-- Generated by pkgdown: do not edit by hand --><html lang="en"><head><meta http-equiv="Content-Type" content="text/html; charset=UTF-8"><meta charset="utf-8"><meta http-equiv="X-UA-Compatible" content="IE=edge"><meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"><title>Functions to Reproduce Clark's Tables — Table65 • ChainLadder</title><!-- favicons --><link rel="icon" type="image/png" sizes="16x16" href="../favicon-16x16.png"><link rel="icon" type="image/png" sizes="32x32" href="../favicon-32x32.png"><link rel="apple-touch-icon" type="image/png" sizes="180x180" href="../apple-touch-icon.png"><link rel="apple-touch-icon" type="image/png" sizes="120x120" href="../apple-touch-icon-120x120.png"><link rel="apple-touch-icon" type="image/png" sizes="76x76" href="../apple-touch-icon-76x76.png"><link rel="apple-touch-icon" type="image/png" sizes="60x60" href="../apple-touch-icon-60x60.png"><script src="../deps/jquery-3.6.0/jquery-3.6.0.min.js"></script><meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"><link href="../deps/bootstrap-5.3.1/bootstrap.min.css" rel="stylesheet"><script src="../deps/bootstrap-5.3.1/bootstrap.bundle.min.js"></script><link href="../deps/font-awesome-6.5.2/css/all.min.css" rel="stylesheet"><link href="../deps/font-awesome-6.5.2/css/v4-shims.min.css" rel="stylesheet"><script src="../deps/headroom-0.11.0/headroom.min.js"></script><script src="../deps/headroom-0.11.0/jQuery.headroom.min.js"></script><script src="../deps/bootstrap-toc-1.0.1/bootstrap-toc.min.js"></script><script src="../deps/clipboard.js-2.0.11/clipboard.min.js"></script><script src="../deps/search-1.0.0/autocomplete.jquery.min.js"></script><script src="../deps/search-1.0.0/fuse.min.js"></script><script src="../deps/search-1.0.0/mark.min.js"></script><!-- pkgdown --><script src="../pkgdown.js"></script><meta property="og:title" content="Functions to Reproduce Clark's Tables — Table65"><meta name="description" content="Print the tables on pages 64, 65, and 68 of Clark's paper."><meta property="og:description" content="Print the tables on pages 64, 65, and 68 of Clark's paper."><meta property="og:image" content="http://mages.github.io/ChainLadder/logo.png"></head><body>
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
      <img src="../logo.png" class="logo" alt=""><h1>Functions to Reproduce Clark's Tables</h1>

      <div class="d-none name"><code>Table65.rd</code></div>
    </div>

    <div class="ref-description section level2">
    <p>Print the tables on pages 64, 65, and 68 of Clark's paper.</p>
    </div>

    <div class="section level2">
    <h2 id="ref-usage">Usage<a class="anchor" aria-label="anchor" href="#ref-usage"></a></h2>
    <div class="sourceCode"><pre class="sourceCode r"><code><span><span class="fu">Table64</span><span class="op">(</span><span class="va">x</span><span class="op">)</span></span>
<span><span class="fu">Table65</span><span class="op">(</span><span class="va">x</span><span class="op">)</span></span>
<span><span class="fu">Table68</span><span class="op">(</span><span class="va">x</span><span class="op">)</span></span></code></pre></div>
    </div>

    <div class="section level2">
    <h2 id="arguments">Arguments<a class="anchor" aria-label="anchor" href="#arguments"></a></h2>
    <p></p>
<dl><dt id="arg-x">x<a class="anchor" aria-label="anchor" href="#arg-x"></a></dt>
<dd><p>an object resulting from <code>ClarkLDF</code> or <code>ClarkCapeCod</code></p></dd>

</dl></div>
    <div class="section level2">
    <h2 id="details">Details<a class="anchor" aria-label="anchor" href="#details"></a></h2>
    <p>These exhibits give some of the details behind the calculations producing
the estimates of future values (a.k.a. "Reserves" in Clark's paper).
Table65 works for both the "LDF" and the "CapeCod" methods.
Table64 is specific to "LDF", Table68 to "CapeCod".</p>
    </div>
    <div class="section level2">
    <h2 id="value">Value<a class="anchor" aria-label="anchor" href="#value"></a></h2>
    <p>A <code>data.frame</code>.</p>
    </div>
    <div class="section level2">
    <h2 id="references">References<a class="anchor" aria-label="anchor" href="#references"></a></h2>
    <p>Clark, David R.,
"LDF Curve-Fitting and Stochastic Reserving: A Maximum Likelihood Approach",
<em>Casualty Actuarial Society Forum</em>, Fall, 2003
<a href="https://www.casact.org/sites/default/files/database/forum_03fforum_03ff041.pdf" class="external-link"> https://www.casact.org/sites/default/files/database/forum_03fforum_03ff041.pdf</a></p>
    </div>
    <div class="section level2">
    <h2 id="author">Author<a class="anchor" aria-label="anchor" href="#author"></a></h2>
    <p>Daniel Murphy</p>
    </div>

    <div class="section level2">
    <h2 id="ref-examples">Examples<a class="anchor" aria-label="anchor" href="#ref-examples"></a></h2>
    <div class="sourceCode"><pre class="sourceCode r"><code><span class="r-in"><span><span class="fu">Table65</span><span class="op">(</span><span class="fu"><a href="ClarkLDF.html">ClarkLDF</a></span><span class="op">(</span><span class="va">GenIns</span>, maxage<span class="op">=</span><span class="fl">20</span><span class="op">)</span><span class="op">)</span></span></span>
<span class="r-out co"><span class="r-pr">#&gt;</span>    Origin CurrentValue FutureValue ProcessSE ProcessCV ParameterSE ParameterCV</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 1       1      3901463    666530.8  209582.5      31.4    156593.2        23.5</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 2       2      5339085   1157423.1  275610.3      23.8    254781.7        22.0</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 3       3      4909315   1365086.0  298810.1      21.9    295848.6        21.7</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 4       4      4588268   1665693.7  329605.5      19.8    353535.0        21.2</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 5       5      3873311   1886481.4  350361.0      18.6    397791.3        21.1</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 6       6      3691712   2514880.3  404131.2      16.1    513654.8        20.4</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 7       7      3483130   3546784.7  479542.3      13.5    698509.6        19.7</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 8       8      2864498   4896501.4  563084.2      11.5    961057.8        19.6</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 9       9      1363294   4991635.1  568138.4      11.4   1218732.2        24.4</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 10     10       344014   6223706.3  634046.3      10.2   2821745.5        45.3</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>     Total     34358090  28914722.7 1369565.1       4.7   4651504.5        16.1</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>     StdError TotalCV</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 1   261622.3    39.3</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 2   375332.8    32.4</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 3   420492.4    30.8</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 4   483349.5    29.0</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 5   530085.6    28.1</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 6   653577.3    26.0</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 7   847275.9    23.9</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 8  1113865.3    22.7</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 9  1344652.2    26.9</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 10 2892103.5    46.5</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>    4848938.3    16.8</span>
<span class="r-in"><span><span class="fu">Table64</span><span class="op">(</span><span class="fu"><a href="ClarkLDF.html">ClarkLDF</a></span><span class="op">(</span><span class="va">GenIns</span>, maxage<span class="op">=</span><span class="fl">20</span><span class="op">)</span><span class="op">)</span></span></span>
<span class="r-out co"><span class="r-pr">#&gt;</span>    Origin CurrentValue CurrentAge AgeUsed GrowthFunction       Ldf TruncatedLdf</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 1                   NA         20    19.5     0.90550697  1.104354     1.000000</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 2       1      3901463         10     9.5     0.77338151  1.293023     1.170841</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 3       2      5339085          9     8.5     0.74418113  1.343759     1.216783</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 4       3      4909315          8     7.5     0.70850094  1.411431     1.278060</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 5       4      4588268          7     6.5     0.66433229  1.505271     1.363033</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 6       5      3873311          6     5.5     0.60892995  1.642225     1.487046</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 7       6      3691712          5     4.5     0.53860005  1.856665     1.681223</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 8       7      3483130          4     3.5     0.44865388  2.228890     2.018275</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 9       8      2864498          3     2.5     0.33421249  2.992108     2.709375</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 10      9      1363294          2     1.5     0.19425429  5.147891     4.661452</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 11     10       344014          1     0.5     0.04743002 21.083696    19.091433</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 12  Total     34358090         NA      NA             NA        NA           NA</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>    LossesAtMaxage FutureValue</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 1              NA          NA</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 2         4567994    666530.8</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 3         6496508   1157423.1</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 4         6274401   1365086.0</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 5         6253962   1665693.7</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 6         5759792   1886481.4</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 7         6206592   2514880.3</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 8         7029915   3546784.7</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 9         7760999   4896501.4</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 10        6354929   4991635.1</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 11        6567720   6223706.3</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 12       63272813  28914722.7</span>
<span class="r-in"><span></span></span>
<span class="r-in"><span><span class="va">X</span> <span class="op">&lt;-</span> <span class="va">GenIns</span></span></span>
<span class="r-in"><span><span class="fu"><a href="https://rdrr.io/r/base/colnames.html" class="external-link">colnames</a></span><span class="op">(</span><span class="va">X</span><span class="op">)</span> <span class="op">&lt;-</span> <span class="fl">12</span><span class="op">*</span><span class="fu"><a href="https://rdrr.io/r/base/numeric.html" class="external-link">as.numeric</a></span><span class="op">(</span><span class="fu"><a href="https://rdrr.io/r/base/colnames.html" class="external-link">colnames</a></span><span class="op">(</span><span class="va">X</span><span class="op">)</span><span class="op">)</span></span></span>
<span class="r-in"><span><span class="fu">Table65</span><span class="op">(</span><span class="fu"><a href="ClarkCapeCod.html">ClarkCapeCod</a></span><span class="op">(</span><span class="va">X</span>, Premium<span class="op">=</span><span class="fl">10000000</span><span class="op">+</span><span class="fl">400000</span><span class="op">*</span><span class="fl">0</span><span class="op">:</span><span class="fl">9</span>, maxage<span class="op">=</span><span class="cn">Inf</span><span class="op">)</span><span class="op">)</span></span></span>
<span class="r-out co"><span class="r-pr">#&gt;</span>    Origin CurrentValue FutureValue ProcessSE ProcessCV ParameterSE ParameterCV</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 1       1      3901463     1323698  284493.9      21.5    340851.2        25.7</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 2       2      5339085     1556996  308547.7      19.8    380563.1        24.4</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 3       3      4909315     1846297  335992.3      18.2    424217.1        23.0</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 4       4      4588268     2209996  367598.9      16.6    471286.9        21.3</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 5       5      3873311     2673721  404330.5      15.1    520246.7        19.5</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 6       6      3691712     3272829  447342.5      13.7    567800.6        17.3</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 7       7      3483130     4053931  497870.8      12.3    607956.3        15.0</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 8       8      2864498     5069599  556756.5      11.0    632156.8        12.5</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 9       9      1363294     6344850  622858.1       9.8    635453.9        10.0</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 10     10       344014     7738201  687857.5       8.9    642428.6         8.3</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>     Total     34358090    36090118 1485499.9       4.1   5169409.8        14.3</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>     StdError TotalCV</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 1   443977.8    33.5</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 2   489928.5    31.5</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 3   541157.1    29.3</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 4   597695.8    27.0</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 5   658892.9    24.6</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 6   722850.5    22.1</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 7   785802.9    19.4</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 8   842377.6    16.6</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 9   889805.5    14.0</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 10  941202.7    12.2</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>    5378615.7    14.9</span>
<span class="r-in"><span><span class="fu">Table68</span><span class="op">(</span><span class="fu"><a href="ClarkCapeCod.html">ClarkCapeCod</a></span><span class="op">(</span><span class="va">X</span>, Premium<span class="op">=</span><span class="fl">10000000</span><span class="op">+</span><span class="fl">400000</span><span class="op">*</span><span class="fl">0</span><span class="op">:</span><span class="fl">9</span>, maxage<span class="op">=</span><span class="cn">Inf</span><span class="op">)</span><span class="op">)</span></span></span>
<span class="r-out co"><span class="r-pr">#&gt;</span>    Origin  Premium CurrentAge AgeUsed GrowthFunction FutureGrowthFactor</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 1               NA        Inf     Inf     1.00000000          0.0000000</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 2       1 1.00e+07        120     114     0.77828466          0.2217153</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 3       2 1.04e+07        108     102     0.74923842          0.2507616</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 4       3 1.08e+07         96      90     0.71365834          0.2863417</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 5       4 1.12e+07         84      78     0.66949336          0.3305066</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 6       5 1.16e+07         72      66     0.61393113          0.3860689</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 7       6 1.20e+07         60      54     0.54317615          0.4568238</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 8       7 1.24e+07         48      42     0.45240260          0.5475974</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 9       8 1.28e+07         36      30     0.33660779          0.6633922</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 10      9 1.32e+07         24      18     0.19489202          0.8051080</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 11     10 1.36e+07         12       6     0.04696731          0.9530327</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 12  Total 1.18e+08         NA      NA             NA                 NA</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>    PremiumxELR FutureValue</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 1           NA          NA</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 2      5970260     1323698</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 3      6209071     1556996</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 4      6447881     1846297</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 5      6686692     2209996</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 6      6925502     2673721</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 7      7164313     3272829</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 8      7403123     4053931</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 9      7641933     5069599</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 10     7880744     6344850</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 11     8119554     7738201</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 12    70449074    36090118</span>
<span class="r-in"><span></span></span>
</code></pre></div>
    </div>
  </main><aside class="col-md-3"><nav id="toc" aria-label="Table of contents"><h2>On this page</h2>
    </nav></aside></div>


    <footer><div class="pkgdown-footer-left">
  <p>Developed by Markus Gesmann, Daniel Murphy, Yanwei (Wayne) Zhang, Alessandro Carrato, Mario Wuthrich, Fabio Concina, Eric Dal Moro.</p>
</div>

<div class="pkgdown-footer-right">
  <p>Site built with <a href="https://pkgdown.r-lib.org/" class="external-link">pkgdown</a> 2.1.1.</p>
</div>

    </footer></div>





  </body></html>

