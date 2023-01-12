<!DOCTYPE html>
<!-- Generated by pkgdown: do not edit by hand --><html lang="en"><head><meta http-equiv="Content-Type" content="text/html; charset=UTF-8"><meta charset="utf-8"><meta http-equiv="X-UA-Compatible" content="IE=edge"><meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"><meta name="description" content='Calculate the matrix of age-to-age factors 
(also called "report-to-report" factors, or "link ratios")
for an object of class triangle.'><title>Calculate Age-to-Age Factors — ata • ChainLadder</title><!-- favicons --><link rel="icon" type="image/png" sizes="16x16" href="../favicon-16x16.png"><link rel="icon" type="image/png" sizes="32x32" href="../favicon-32x32.png"><link rel="apple-touch-icon" type="image/png" sizes="180x180" href="../apple-touch-icon.png"><link rel="apple-touch-icon" type="image/png" sizes="120x120" href="../apple-touch-icon-120x120.png"><link rel="apple-touch-icon" type="image/png" sizes="76x76" href="../apple-touch-icon-76x76.png"><link rel="apple-touch-icon" type="image/png" sizes="60x60" href="../apple-touch-icon-60x60.png"><script src="../deps/jquery-3.6.0/jquery-3.6.0.min.js"></script><meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"><link href="../deps/bootstrap-5.2.2/bootstrap.min.css" rel="stylesheet"><script src="../deps/bootstrap-5.2.2/bootstrap.bundle.min.js"></script><!-- Font Awesome icons --><link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.12.1/css/all.min.css" integrity="sha256-mmgLkCYLUQbXn0B1SRqzHar6dCnv9oZFPEC1g1cwlkk=" crossorigin="anonymous"><link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.12.1/css/v4-shims.min.css" integrity="sha256-wZjR52fzng1pJHwx4aV2AO3yyTOXrcDW7jBpJtTwVxw=" crossorigin="anonymous"><!-- bootstrap-toc --><script src="https://cdn.jsdelivr.net/gh/afeld/bootstrap-toc@v1.0.1/dist/bootstrap-toc.min.js" integrity="sha256-4veVQbu7//Lk5TSmc7YV48MxtMy98e26cf5MrgZYnwo=" crossorigin="anonymous"></script><!-- headroom.js --><script src="https://cdnjs.cloudflare.com/ajax/libs/headroom/0.11.0/headroom.min.js" integrity="sha256-AsUX4SJE1+yuDu5+mAVzJbuYNPHj/WroHuZ8Ir/CkE0=" crossorigin="anonymous"></script><script src="https://cdnjs.cloudflare.com/ajax/libs/headroom/0.11.0/jQuery.headroom.min.js" integrity="sha256-ZX/yNShbjqsohH1k95liqY9Gd8uOiE1S4vZc+9KQ1K4=" crossorigin="anonymous"></script><!-- clipboard.js --><script src="https://cdnjs.cloudflare.com/ajax/libs/clipboard.js/2.0.6/clipboard.min.js" integrity="sha256-inc5kl9MA1hkeYUt+EC3BhlIgyp/2jDIyBLS6k3UxPI=" crossorigin="anonymous"></script><!-- search --><script src="https://cdnjs.cloudflare.com/ajax/libs/fuse.js/6.4.6/fuse.js" integrity="sha512-zv6Ywkjyktsohkbp9bb45V6tEMoWhzFzXis+LrMehmJZZSys19Yxf1dopHx7WzIKxr5tK2dVcYmaCk2uqdjF4A==" crossorigin="anonymous"></script><script src="https://cdnjs.cloudflare.com/ajax/libs/autocomplete.js/0.38.0/autocomplete.jquery.min.js" integrity="sha512-GU9ayf+66Xx2TmpxqJpliWbT5PiGYxpaG8rfnBEk1LL8l1KGkRShhngwdXK1UgqhAzWpZHSiYPc09/NwDQIGyg==" crossorigin="anonymous"></script><script src="https://cdnjs.cloudflare.com/ajax/libs/mark.js/8.11.1/mark.min.js" integrity="sha512-5CYOlHXGh6QpOFA/TeTylKLWfB3ftPsde7AnmhuitiTX4K5SqCLBeKro6sPS8ilsz1Q4NRx3v8Ko2IBiszzdww==" crossorigin="anonymous"></script><!-- pkgdown --><script src="../pkgdown.js"></script><meta property="og:title" content="Calculate Age-to-Age Factors — ata"><meta property="og:description" content='Calculate the matrix of age-to-age factors 
(also called "report-to-report" factors, or "link ratios")
for an object of class triangle.'><meta property="og:image" content="http://mages.github.io/ChainLadder/logo.png"><!-- mathjax --><script src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js" integrity="sha256-nvJJv9wWKEm88qvoQl9ekL2J+k/RWIsaSScxxlsrv8k=" crossorigin="anonymous"></script><script src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/config/TeX-AMS-MML_HTMLorMML.js" integrity="sha256-84DKXVJXs0/F8OTMzX4UR909+jtl4G7SPypPavF+GfA=" crossorigin="anonymous"></script><!--[if lt IE 9]>
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
      <img src="../logo.png" class="logo" alt=""><h1>Calculate Age-to-Age Factors</h1>
      
      <div class="d-none name"><code>ata.rd</code></div>
    </div>

    <div class="ref-description section level2">
    <p>Calculate the matrix of age-to-age factors 
(also called "report-to-report" factors, or "link ratios")
for an object of class <code>triangle</code>.</p>
    </div>

    <div class="section level2">
    <h2 id="ref-usage">Usage<a class="anchor" aria-label="anchor" href="#ref-usage"></a></h2>
    <div class="sourceCode"><pre class="sourceCode r"><code><span><span class="fu">ata</span><span class="op">(</span><span class="va">Triangle</span>, NArow.rm <span class="op">=</span> <span class="cn">TRUE</span>, colname.sep <span class="op">=</span> <span class="st">"-"</span>,</span>
<span>        colname.order<span class="op">=</span><span class="fu"><a href="https://rdrr.io/r/base/c.html" class="external-link">c</a></span><span class="op">(</span><span class="st">"ascending"</span>,<span class="st">"descending"</span><span class="op">)</span><span class="op">)</span></span></code></pre></div>
    </div>

    <div class="section level2">
    <h2 id="arguments">Arguments<a class="anchor" aria-label="anchor" href="#arguments"></a></h2>
    <dl><dt>Triangle</dt>
<dd><p>a loss "triangle". Must be a <code>matrix</code>.</p></dd>

    <dt>NArow.rm</dt>
<dd><p><code>logical</code> indicating if 
        rows of age-to-age (ata) factors that are all <code>NA</code>
        should be removed.
        "All-NA" rows typically occur for the most recent origin year
        of a loss triangle.</p></dd>

    <dt>colname.sep</dt>
<dd><p>a <code>character</code> indicating the separator
        character to place between the column names of <code>Triangle</code>
        that will be used to lable the columns of the resulting 
        matrix of ata factors</p></dd>

    <dt>colname.order</dt>
<dd><p>"ascending" indicates that the less mature
        age comes first in the column labels of the ata matrix</p></dd>

    </dl></div>
    <div class="section level2">
    <h2 id="details">Details<a class="anchor" aria-label="anchor" href="#details"></a></h2>
    <p><code>ata</code> constructs a matrix of age-to-age (ata) factors resulting
from a loss "triangle" or a <code>matrix</code>.
Simple averages and volume weighted averages are saved as 
"smpl" and "vwtd" attributes, respectively.</p>
    </div>
    <div class="section level2">
    <h2 id="value">Value<a class="anchor" aria-label="anchor" href="#value"></a></h2>
    

<p>A <code>matrix</code> with "smpl" and "vwtd" attributes.</p>
    </div>
    <div class="section level2">
    <h2 id="author">Author<a class="anchor" aria-label="anchor" href="#author"></a></h2>
    <p>Daniel Murphy</p>
    </div>
    <div class="section level2">
    <h2 id="see-also">See also<a class="anchor" aria-label="anchor" href="#see-also"></a></h2>
    <div class="dont-index"><p><code><a href="summary.ata.html">summary.ata</a></code>, <code><a href="print.ata.html">print.ata</a></code> and <code><a href="chainladder.html">chainladder</a></code></p></div>
    </div>

    <div class="section level2">
    <h2 id="ref-examples">Examples<a class="anchor" aria-label="anchor" href="#ref-examples"></a></h2>
    <div class="sourceCode"><pre class="sourceCode r"><code><span class="r-in"><span><span class="fu">ata</span><span class="op">(</span><span class="va">GenIns</span><span class="op">)</span></span></span>
<span class="r-out co"><span class="r-pr">#&gt;</span>       dev</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> origin   1-2   2-3   3-4   4-5   5-6   6-7   7-8   8-9  9-10</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>   1    3.143 1.543 1.278 1.238 1.209 1.044 1.040 1.063 1.018</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>   2    3.511 1.755 1.545 1.133 1.084 1.128 1.057 1.086    NA</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>   3    4.448 1.717 1.458 1.232 1.037 1.120 1.061    NA    NA</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>   4    4.568 1.547 1.712 1.073 1.087 1.047    NA    NA    NA</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>   5    2.564 1.873 1.362 1.174 1.138    NA    NA    NA    NA</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>   6    3.366 1.636 1.369 1.236    NA    NA    NA    NA    NA</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>   7    2.923 1.878 1.439    NA    NA    NA    NA    NA    NA</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>   8    3.953 2.016    NA    NA    NA    NA    NA    NA    NA</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>   9    3.619    NA    NA    NA    NA    NA    NA    NA    NA</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>   smpl 3.566 1.746 1.452 1.181 1.111 1.085 1.053 1.075 1.018</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>   vwtd 3.491 1.747 1.457 1.174 1.104 1.086 1.054 1.077 1.018</span>
<span class="r-in"><span></span></span>
<span class="r-in"><span><span class="co"># Volume weighted average age-to-age factor of the "RAA" data</span></span></span>
<span class="r-in"><span><span class="va">y</span> <span class="op">&lt;-</span> <span class="fu"><a href="https://rdrr.io/r/base/attr.html" class="external-link">attr</a></span><span class="op">(</span><span class="fu">ata</span><span class="op">(</span><span class="va">RAA</span><span class="op">)</span>, <span class="st">"vwtd"</span><span class="op">)</span></span></span>
<span class="r-in"><span><span class="va">y</span></span></span>
<span class="r-out co"><span class="r-pr">#&gt;</span>      1-2      2-3      3-4      4-5      5-6      6-7      7-8      8-9 </span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 2.999359 1.623523 1.270888 1.171675 1.113385 1.041935 1.033264 1.016936 </span>
<span class="r-out co"><span class="r-pr">#&gt;</span>     9-10 </span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 1.009217 </span>
<span class="r-in"><span><span class="co"># "To ultimate" factors with a 10% tail</span></span></span>
<span class="r-in"><span><span class="va">y</span> <span class="op">&lt;-</span> <span class="fu"><a href="https://rdrr.io/r/base/rev.html" class="external-link">rev</a></span><span class="op">(</span><span class="fu"><a href="https://rdrr.io/r/base/cumsum.html" class="external-link">cumprod</a></span><span class="op">(</span><span class="fu"><a href="https://rdrr.io/r/base/rev.html" class="external-link">rev</a></span><span class="op">(</span><span class="fu"><a href="https://rdrr.io/r/base/c.html" class="external-link">c</a></span><span class="op">(</span><span class="va">y</span>, <span class="fl">1.1</span><span class="op">)</span><span class="op">)</span><span class="op">)</span><span class="op">)</span></span></span>
<span class="r-in"><span><span class="fu"><a href="https://rdrr.io/r/base/names.html" class="external-link">names</a></span><span class="op">(</span><span class="va">y</span><span class="op">)</span> <span class="op">&lt;-</span> <span class="fu"><a href="https://rdrr.io/r/base/paste.html" class="external-link">paste</a></span><span class="op">(</span><span class="fu"><a href="https://rdrr.io/r/base/colnames.html" class="external-link">colnames</a></span><span class="op">(</span><span class="va">RAA</span><span class="op">)</span>, <span class="st">"Ult"</span>, sep<span class="op">=</span><span class="st">"-"</span><span class="op">)</span></span></span>
<span class="r-in"><span><span class="va">y</span></span></span>
<span class="r-out co"><span class="r-pr">#&gt;</span>    1-Ult    2-Ult    3-Ult    4-Ult    5-Ult    6-Ult    7-Ult    8-Ult </span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 9.812257 3.271452 2.015033 1.585531 1.353218 1.215409 1.166493 1.128940 </span>
<span class="r-out co"><span class="r-pr">#&gt;</span>    9-Ult   10-Ult </span>
<span class="r-out co"><span class="r-pr">#&gt;</span> 1.110138 1.100000 </span>
<span class="r-in"><span></span></span>
<span class="r-in"><span><span class="co">## Label the development columns in "ratio-type" format</span></span></span>
<span class="r-in"><span><span class="fu">ata</span><span class="op">(</span><span class="va">RAA</span>, colname.sep<span class="op">=</span><span class="st">":"</span>, colname.order<span class="op">=</span><span class="st">"desc"</span><span class="op">)</span></span></span>
<span class="r-out co"><span class="r-pr">#&gt;</span>       dev</span>
<span class="r-out co"><span class="r-pr">#&gt;</span> origin    2:1   3:2   4:3   5:4   6:5   7:6   8:7   9:8  10:9</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>   1981  1.650 1.319 1.082 1.147 1.195 1.113 1.033 1.003 1.009</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>   1982 40.425 1.259 1.977 1.292 1.132 0.993 1.043 1.033    NA</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>   1983  2.637 1.543 1.163 1.161 1.186 1.029 1.026    NA    NA</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>   1984  2.043 1.364 1.349 1.102 1.113 1.038    NA    NA    NA</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>   1985  8.759 1.656 1.400 1.171 1.009    NA    NA    NA    NA</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>   1986  4.260 1.816 1.105 1.226    NA    NA    NA    NA    NA</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>   1987  7.217 2.723 1.125    NA    NA    NA    NA    NA    NA</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>   1988  5.142 1.887    NA    NA    NA    NA    NA    NA    NA</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>   1989  1.722    NA    NA    NA    NA    NA    NA    NA    NA</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>   smpl  8.206 1.696 1.315 1.183 1.127 1.043 1.034 1.018 1.009</span>
<span class="r-out co"><span class="r-pr">#&gt;</span>   vwtd  2.999 1.624 1.271 1.172 1.113 1.042 1.033 1.017 1.009</span>
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

