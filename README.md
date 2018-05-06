<html>
<head>

</head>
<body>
<div id="header">

<!--CofC Logo-->
<a href="http://www.cofc.edu"><img src="https://lh3.googleusercontent.com/m-VgMx9Rzpj_Sx_ca88CPlD4_QbONpnfJo4I0rTiL1rn8o7lMOcP-0sys7EslT4L4JzUP2Mw2fw1hcJw645yRYwT5kitBkUUEYHkYbNs4RgTQeWS4oxunNtAz1HCsOnQGeQGYWdjdrVLy-u72SPg5dIeVxkQfkphhFb52oR5Iu2i2TK6RbvYNqKCIZCkAP0wH3vWf6-k7CtgYRcKHJUdi9-Z1fvsVDjKGG2PQIcYuQFRMhAKHL8Jbt4h13WLPJkOf78VU9cy7oTv4J77eCc1lrkTMYBNjBVJsfrZB_f5wViRxXTntXl5c84Rkt4aU7C1Kh08ugurmjJqBay1OOjPv66TLxNJSZmQs_MlHDikbrLvcgVCzoWyUkEDOzZFshD-xnZE-Jb-6rMmo8tUjMLrcZ_hR2G_w2NH7di5CHcWArUZnsG_o0OcNYSl7p5Jx6GISZfkMUkwLoRzU4eajd-kI9mwvO94BgefCUbpr1TwF-oRUSCv6nKDeAdcnElzTH86xiq-vIRMGiBK-1GgA7Liwe-GJGYXotaWA1iAVHjkrjoAcYjWAFdZv5PzfHypUSliCz4V1bdF3GK_TLjG8R6MV64QwvXI1lqeJwU4sQEO=s160-no" height="50" align="left" hspace="10px"></a>

<!--Benefitfocus Logo-->
<a href="https://www.benefitfocus.com/"><img src="https://www.benefitfocus.com/sites/default/files/styles/logo__retina_/public/media/images/Benefitfocus_Benefits_are_good_outlines_RGB.png?itok=yDJSTYsb&timestamp=1433868479" height="50" align="right" hspace="10px"></a>

<!--Project Title-->
<h2><a id="Understanding the Impact of Medical Plan Design on Healthcare Utilization" class="anchor" aria-hidden="true"><span class="octicon octicon-link"></span></a>Understanding the Impact of Medical Plan Design on Healthcare Utilization</h2>
<h3><a id="A Partnership with the College of Charleston and Benefitfocus" class="anchor" aria-hidden="true"><span class="octicon octicon-link"></span></a>A Partnership with the College of Charleston and Benefitfocus</h3>
</div>
<br>

<!--Contributors-->
<h3>Contributors</h3>
<table style="width:50%">
  <tr>
    <td>Alex Giarrocco</td>
    <td>Sonia Kopel</td>    
    <td>Neal Sakash</td>
  </tr>
  <tr>
    <td><a href="https://github.com/g-rock"><img src="https://avatars2.githubusercontent.com/u/8891104?s=400&v=4" height="100" align="center" hspace="10px"></a></td>
    <td><a href="https://github.com/sooonia"><img src="https://avatars2.githubusercontent.com/u/16102850?s=400&v=4" height="100" align="center" hspace="10px"></a></td>
    <td><a href="https://github.com/npsakash"><img src="https://avatars0.githubusercontent.com/u/16137166?s=400&u=0f42f218054a840c726471d131017879fc6870a3&v=4" height="100" align="center" hspace="10px"></a></td>
          
  </tr>
</table>
        
<br>
    
<!--Contents-->
<h3>Contents</h3>
<ul>
<li><a class="active" href="#source-material"><u>Source Material</u></a></li>
<li><a class="active" href="#project-details">Project Details</a></li>
<li><a class="active" href="#results">Results</a></li>
<li><a class="active" href="#introduction">Introduction</a></li>
<li><a class="active" href="#data-used">Data Used</a></li>
<li><a class="active" href="#technologies-used">Technologies Used</a></li>
<li><a class="active" href="#scrum-timeline">Scrum Timeline</a></li>
<li><a class="active" href="#variable-correlations">Variable Correlations</a></li>
<li><a class="active" href="#hospitalization-model">Hospitalization Model</a></li>
<li><a class="active" href="#behavior-models">Behavior Models</a></li>
<li><a class="active" href="#challenges-triumphs">Challenges and Triumphs</a></li>
</ul>
</div>
 <br>

<div id="section">

<h3><a id="source-material" class="anchor" href="#source-material" aria-hidden="true"><span class="octicon octicon-link"></span></a>Source Material</h3>
<dl>
<!-- <dt><a href="https://drive.google.com/drive/folders/1Nt3z3YmBd0J0gH0AGkhc1xPe7p3CC6ZS">Benefitfocus Flyer</a></dt> -->
<dt><a href="https://meps.ahrq.gov/mepsweb/index.jsp">Medical Expenditure Panel Survey</a>
    <dd>
    <a href="https://meps.ahrq.gov/mepsweb/data_stats/download_data_files_results.jsp?cboDataYear=All&cboDataTypeY=1%2CHousehold+Full+Year+File&buttonYearandDataType=Search&cboPufNumber=All&SearchTitle=Consolidated+Data">2015 Full Year Consolidated Data File</a></dd>
<dd><a href="https://meps.ahrq.gov/mepsweb/data_stats/download_data_files_results.jsp?cboDataYear=All&cboDataTypeY=1%2CHousehold+Full+Year+File&buttonYearandDataType=Search&cboPufNumber=All&SearchTitle=Person+Round+Plan">2015 Person Round Plan Public Use File</a></dd>
</dt>
</dl>

<h3>
<a id="project-details" class="anchor" href="#project-details" aria-hidden="true"><span class="octicon octicon-link"></span></a>Project Details</h3>

<p>
<b>Benefitfocus Deliverable:</b> 
<br>
"The team will need to explore models for computing the impact of various medical benefit plan attributes
(deductible, coinsurance, copay, etc.) on healthcare utilization and personal well-being. The team will need to
research and define what is meant by personal well-being. Using those models, the team will build an interactive
web app enabling a user to visualize the relationships between medical plan design values and healthcare
utilization and well-being. The students will have the freedom to use modeling techniques, programing languages,
and frameworks of their own choice."
</p> 
<h3>
<a id="results" class="anchor" href="#results" aria-hidden="true"><span class="octicon octicon-link"></span></a>Results</h3>
<p>
With the guidance of Benefitfocus, our team set out to find a link between health plan design, patient behavior, and healthcare utilization. Due to the single semester time constraint we decided to narrow the scope of the project to only focus on utilization instead of personal well-being. We chose the most costly healthcare expenditure, Inpatient Hospitalization, as our primary target variable and were successful with building a classification model that predicted this utilization from patient behavior with accuracy above 88%.
</p> 
<h3>
<a id="introduction" class="anchor" href="#introduction" aria-hidden="true"><span class="octicon octicon-link"></span></a>Introduction</h3>

<p>
Healthcare spending is one the highest consumer costs in the United States. At $3.3 trillion, it accounted for almost a fifth of the country's GDP in 2016. This share has nearly doubled from 30 years ago. Relative to a 23% consumer price index increase from 2005, 2014 healthcare costs rose 15 percentage points. Of the all major goods and services, only child-care and higher education have experienced price increases faster than healthcare. Externalities from this increased spending can contribute to slow wage growth, temporary or part-time employment, outsourcing, a reduced to care, and personal bankruptcies.
</p>
<p>
Navigating modern healthcare policy has become increasingly daunting for both employers and employees. With employers strained by rising costs, employees are now expected to take greater responsibility with choosing, managing, and paying for their coverage.
</p>
<img src="https://lh3.googleusercontent.com/HCGjbYoDDz4UZI5H0JA90pymh4VSqSfM3_Wvxxu8h79Tor5nGtdNBsTF2f4_KBd-8uUaztqyu0zNikECbgH7BG1qU0L8YmIiHxsYktrdHLmr4eRxsb9MkhMw7oH_iVubllZo8XwbahJvF3U8mexuwCRZGroP-A-vLjwW3qCCZsVmMHphVPbn8Z6L2VldNZ1nUv5WYcztuwlR9sLqOHkFW7guIC9wFHGhgcGGDBJvNXtfSugKf12OrjF167PCO4bD5eWT6h8xdks_G4SYySPS5ZjM1I6q3e1Ovxbm4mop0PoB1WWbhUKwifA_pSVsT3t0Ua0arGDKfWzqF0Qr0YrUtm9Cjz7KlbuKa3cG-N-l-iHtoJ0lZwy1UEAkMgttbIyynmrAXCYalT0n16gW_4tA_45GpnKrm2hKk-cSFFAkgW_p-c-pXtVBnJnX1LOB-fZOkq_WbXlzuoj-SfkqNABo0rAzaT7dqmiaSgS4renrTrT6HDkY8lNdIvXGGRQXB1MrVWjvZcwhsIC9rxoXYCVMsinskkAHmKRzsIhFbh7yYK9g9Dwd3ONcaeLOJMzkZtH0Q9eQ7eSpQWpafpYJcL1f-Pwnzdhvfj42jglAZy14=w892-h502-no" alt="Problem/Solution" align="right" height=350>
<p>
Hospitalizations have historically been the largest expense in healthcare, accounting for nearly a third of all costs in 2016. If the rate of hospitalizations were to decrease, savings from the reduced spending could be passed on to employer and employee premiums.
</p>
    <p>Benefitfocus seeks to mediate this expenditure and has partnered with the College of Charleston to help predict inpatient hospitalizations. Our team has built a model to predict this expenditure and have found a link between a patient's behavior and their chances of being hospitalized. We further hoped to determine if this behavioral link could be an extension of the patient's plan design.</p>

<h3>
<a id="data-used" class="anchor" href="#data-used" aria-hidden="true"><span class="octicon octicon-link"></span></a>Data Used
</h3>
<img src="https://meps.ahrq.gov/mepstrends/src/custom/img/oc_meps_logo_blue.png" alt="MEPS Logo" align="center" height=50>
<p>
For our predictive model we used data from the publicly available and federally administered Medical Expenditure Panel Survey. The specific dataset used came from the 2015 consolidated survey of families and individuals, their medical providers, and employers across the US. The dataset includes specific health services used, how frequently they were used, the cost of these services, and how they were paid for. From this dataset we were able to parse out our predictor, control, and target variables relating to plan design, patient's behavior, and hospitalizations. The MEPS survey has influenced every major US healthcare policy decision since its inception in 1996.</p> 


<h3>
<a id="technologies-used" class="anchor" href="#technologies-used" aria-hidden="true"><span class="octicon octicon-link"></span></a>Technologies Used</h3>

<p>R was out primary programming language used for project's statistical computation and modeling. RStudio was our R IDE, using dplyr to streamline data manipulation and ggplot2 for data visualizations. RShiny was used to develop our interactive web application to display results. GitHub was used for team collaboration, development, and version control. ZenHub is the agile project management tool integrated with GitHub.</p>
<img src="https://lh3.googleusercontent.com/dX9vQYsRv5BHHL1h4gYiUC_jwhenuP52qSI0FbVUs-l7w1ckiDQsC32LcYnjCTKZ5YRNYSB6JxLRlk3kvlnxpMFtBm8DkMlHFhBefZixq9BkGNBdUEj84Ob5IW6cf4ZE-kDa40voamNUsojnzkv7nqyIevgv4e9d8K0ZyauygRWbtxHiZyIitA-G0dPnXj9R37K1AqBRy3I-7V5zXNq2jc8TOrGHFLqaw_qqBiA-tR-B84-fF86SpsOWk3yLamxeLuVP9MH5buRy1ZKn4zQ0mdh8vHkYByzcSTNjO7RqdwF6TccuC1RdBz3r8Rz2VN5X-BWp3Q77uxE-Pp4NvDX2_L0FrKvCwigZM3X13XhqmhZl4NSW9JurXUQJBaxcGV14ejGMZ7km75X86f1I5EJFq92iVM0-GDQ-FlqKMeX7dA396oeaIHGhHdgJPCvvYgdcpjrg3f2xFxfvJYLOKqqENLqDaRn6dBY6AaVwxbKw3efvIHUnuQRje-3m8xeNAP5rjUtzaRQ47re7C7BPGeFQI7i8jB_His-4ZFgf4FH-tSEbRjKeEs8Rw1TjFoFxv0HMxD2lXsCW3htrsXWwP9mWf54JB-DadIEQqVEW2M4l=w614-h346-no" alt="Technologies Used" align="center" height=350>

<h3>
<a id="scrum-timeline" class="anchor" href="#scrum-timeline" aria-hidden="true"><span class="octicon octicon-link"></span></a>Scrum Timeline</h3>
<p>For the agile development process we broke the project into six sprints, with weekly correspondence with the team at Benefitfocus and stand-ups with our professor and advisor, Dr. Paul Anderson.</p>
<p>
<img src="https://lh3.googleusercontent.com/GyycC3zPvmnRArRgAPzMVhmCXZw6xZR8S9NAxSQZpbjKW1Bxr-Ug3xPQp45ndgIh9OD54hk6-20-4F0zznCpXKS_gDTiwrvQCpiS9jxsSXpNQjXhlfJUbWyFyK56ytFwj_zPl3494RzACT-Vs1ylPbQw5_sjw3Llm6-ZewVKIusdb3OePWYLqix7sAjROXaxSf1I3gPnp78aL8KJsX_xk5fABTdX3EMF3yCqerfF3D8X5DytJg7nHKRT1f3_xH1ptOoWpEmw4TimIgh4yTmxe6QfXh5eiiZqnB4qJ39ixFAMm-rlWsztCuf37ernQfTzQlhqYDB7aHq2hxlBK7ZooBTrl9xTmAn0DFDaFrjt9ecSuYlkr764pgTT8w5q1CDKBRuDBOoATjxypFkCtBwNPb29cyfpblUCi3UfqfJlXSG8z1V5V4irrryLqO9UNF42X6ozwk29tTI4e7NNS6J0pKnp2GYXJmdimjdJVBGi6HMFenYOWxue_TpBpy2iJSD78nueZV4cAt9I_KcqzABrbpLHa6P1PnJstAxqOexiGaa92S1VM9c2EcEMtFel4v8vGSp_iC83A29L8meArdC_UdqZ35N53ViU8-CNZMSq=w614-h346-no" alt="Scrum Timeline" height=350>
<br>


<h3>
<a id="variable-correlations" class="anchor" href="#variable-correlations" aria-hidden="true"><span class="octicon octicon-link"></span></a> Variable Correlations</h3>

<p>
To gain a top-level understanding of the data set, we generated variable correlation plots using all of the numeric and ordered variables. It is evident from these plots that behaviors are generally correlated with one another. Given that this is the case, it is possible that this will affect out variable importance later on in modelling as some of the predictors may be giving the same information. 
</p>
    <img src="https://lh3.googleusercontent.com/14ABmmkpTF75B-3Wwl7Qh9qXTzwh1IOPvY4jYQDqZvwWC1uWnZhn7XgnxG0BVWDrBVUUH3Yl7VmoFvcA6eLo3PL27PqhlgrMRUpxyAwfdrc4_1R4NK0lcwcvnPlSRA7uAGhqaDNWCoDx_VqG_VPXzjRVihblxeSK84ZRR6WXIDpjigkWKirXHgfXnv-DG0mkPLoFV0wQDwB4ywHO9dDJ2z_ZwIAOb6dStOeZx734xvHyydHml5_hryZfi4Ja-DfETWSHMbXzTGR0ckuqgHAanFTDqQJKA-ozr61nIpvAORPrE4rjUim4l43aGchz3Hoy1tEynOaGwxcbbR8bVmGYJJkNQgNravJ9qgsUPyK5Z63VBqfUlLx5c2yOoIvKek-tyYwKJMMNmRVwlZ9R7Xx7ejczHV2WUZW0qxui7BlDob5OqVOJLrr8gTyFceb0Xes-9AHPMcxVPmbDbXHFx6gsGfAmDpb8k03k7GHwdoSa08_e0YlP2XW25PtfsGcwqY4Mm9_p4IYMK1BgsImEfNReKB0zoJPKk5hED4FydtqHW1wYRPjHgFwg02hYGnGOvx1MYmLZYAYNUi9c_kVed9EJuvXaov_i-Pruw2R1SvdQ=w1204-h677-no" alt="Variable Correlation All +40" align="" height=350>
<br>
<p>
We noticed weaker correlations among papsmear, breast exam, and mamogram. Because men do not receive those exams, we subset the data set by gender to confirm our results that behaviors are strongly correlated. In the only women subset plot, the correlations are stronger for those variables.

</p>
    <img src="https://lh3.googleusercontent.com/IIbNQCWlm9UOmXZIxuKjMlQAYiQIYFPcHqYfI5LFsFS-zpn8KcuCHobq662HXjWzMgczb5roeJ10XRgVPZwz7pmW9LCpul7yxfFow1M7ZGfBhry2VD7JdajnuXx8kczlGF4PptiNGocoSWKl17JZoiFNbekkq60vWLoG4enXC3RPc9s-zX0v7TzeBtyM54KonKt4dy-6yw8PjSn2Oh_db5WVLN1NRwLt8H132PPCCJfC1bocSErcmRQ-YFH3QVwTURlfXnP1SLxQrqRxMtyNWP_pqGxh_KnTdvcXpXnDq10yRzFcbNjXYVmf5Prm8V50Aa7S_XNx38o2WnY0KUTte8J-DuCmRN4-LB1xonC0XDjI2PgM_KRjjJXLXmVvXOp08ttuRzyp3yM1X3Zzpe-UI3jPRgCsvjlucbLxU6WC3wSF8xjMFApAfp5WQa95ySggGWednzQnvOUKVNInRxqLpP9ddp5DSiOFvrHTnqMfjtyt279KEPeao72-oW7E85IyQhHyB3HoSIwuztwCA4SQJp98Flp4nUS_EOBo5lk5I3foY-3sNTzthla7UhZHMBHKvRFW3p4onVjB22gBesbVUUqxV9jSQAgCNp0FDmEU=w1204-h677-no" alt="Variable Correlation by Gender +40" align= "center" height=350>
<br>

<h3>
<a id="hospitalization-model" class="anchor" href="#hospitalization-model" aria-hidden="true"><span class="octicon octicon-link"></span></a>Hospitalization Model</h3>
<p>
Our best model is a random forest that predicts hospitilization using various predictor variables such as Behaviors, Controls, and Plan Design. This is a high performing model compared to a baseline model that predicts the majority class (non-hospilization) 
</p>
<br>
<p>
  <img src="https://lh3.googleusercontent.com/P8r2hkpMW5Kb6le9cYqxwXGIGxHO4SPbixCb67gGUennbeTrjCPkmvH3J-4g9Dg65SXzHC5gqz7mq8l1AEQopIMdIkOuipqRHWjiZ_fZLDxkQk-5oyWwGiUHtoZ-pNhCK-A-6Sp2X8t0KyAJ68I7gHJ4a66hIqE0BXlokcFvQiWBVfeyBfzXdwdaMdW_6Kc-IhWaU-nt-fj6RoedT2uMYnnsBtu3QOZ1p06-H77F_0rd2LsP7Qs3WmqHqayrh2GZpVnEQbQYimICLDWpN4q4tPIYmiV1R2hG4D_2g83ZAkzCRkPq54VAMF632U2Eav5YZiIqLtib1Q4W1Iee8fAnlc9KBsfBFJRR7z8Gf6W7ZLwZhgpaAmvN_36eyEaAo-KBVtlC_6e1BNgBmBzpoyw_akjUwjb5Al5LzFjgCQjCWQohEkbgIu23PsXhoszlW63Fu0yv6d8n1VRivI4I9JGrjvxBMG9EamgXHKMTmuzlMVDAsYlnssaKwsFb-eU1Eg7PliHMRwvftw6SQtpDPG7IcFq5Pljc7FjJrjCxW5uIg6rREbLkW9zvWWE-s8TeSb8z1JyLvVGk4EeaChL40oWyZBliIdvLLhkGOWQRPHhO=w1204-h677-no" alt="Hospitalization Model Two" align="" height=350>
</p>
<br>
<h3>
<a id="behavior-models" class="anchor" href="#behavior-models" aria-hidden="true"><span class="octicon octicon-link"></span></a>Behavior Models</h3>
<p> We then extended this approach to predict behaviors instead of hospitalization. Due to time constraints, we were not able to tune these models to the performance we wanted. However, the models to offer a little predictive power. In the future, it may be best to coerce the behaviors down to fewer levels. However, some of the models did pick out plan design variables as important when predicting behavior.  
</p>
<img src="https://lh3.googleusercontent.com/irnrvx6RzWuhqknQGN-7AvHH4vRXUzr_0pRbrD_cUZ_x41FYWcQi4I4LzXJpZJc2Szw8d9-cXy--DduZNKFuDUbtgRb5wBhyqBavhhLONorh77pNznoZov1BD1uVEmbmYH18HaJzfP6or_7wyr3-gQeHKP7spaYT6tNKowGuXifUsTXU7SoWONlFsVBA6zutWVCKNy737dgUZdXvO5iHVdykypgHsXdiqhfPcAj2yi_gnZgV0B5DrNHPS-_qe9ZpDdk2dvIlGDxJUgGvNtibB-QPqvPT9sNhuLtdA25fjW7Aj95fAB4-0RAoKgZexoUzb-SrggwKgQGvnoCp7hSl5eJbYQs36gUR_pEq27dQDZH1DYpQU6gYDiYWhn1pDGRq9bv9nFO3jX_tcOkrgqwTTyujJqEs1PgCxHxH350H7kQZMWt7Bo4lKofZQNLzVNxGSxz0w4hBlA0icNMDfmWRf-0nKN8etmoW-hBBZ89DPUZuIELIlL69xoBUlffujgkphlo6Ydiekht8WK3pBqTXYMvg5OCSSlec4444ut5xDuveqeqfPXqDHKOHqa4d1QyTFaAorJmdCR4Eas_sQb9GJZLq5y0SDsTC0o0EGJ2E=w1204-h677-no" alt="Behavior Model One" align="" height=350>
<br>
<p>
  Our team further wanted to see if certain preventive care measures could be grouped together in order to simplify our prediction. We summarized the behaviors of respondents by those who follow or do not follow CDC guidelines and divided age into three categories: less than 40, 40-60, and greater than 60. We, however, did not see a greater improvement using this generalization, since it appears someone who is already getting regular checkups is also following through with recommended preventive care.
 </p>
 <img src="https://lh3.googleusercontent.com/Gilz6sJsruJcCRonp4jguJBjaci9BzeaEJsrsjvov0lN1e3NZOloPbnNKLqWTRcUcpf5YMp45psOTiMpGrppgKybGZ0K3T9sv8ao_F9ZKGL8ARFGNhSukidki2VOWgX89gu2TfFJ5B2zirlag1N6tpGPRoy03PDi9tE2zzFdo1uqO2mhUt86e2B4tu3Cn56d99KkM0S8k29_hhuYQy2q62e4ZmnF0Hs9JFm07Gd9pVLaSJrZ-AXJL6U5Ws9iPUP3apgYu4lH0WQf20nll1we6PIrpP9MngYmnTOmPBUMwC9ERDvit6TS7ZLzRqxg7bTgdM4cu-fH-VdXZ_5foO-3eAjGh5VdovFG20FpQmxnB5dLYG0BldFY9bOyn2cJAaLnOGaE-6ST04r80QmB-XtSXnWqNPOx-atLsxMi0u-vqDkbcfCSiLUO5P545Uaprr1eGiV3Zg6NETUWIVUPA3b-EoffsH-Jz6LdRryQgTVncjLK5ZCp-SnbsNfMCU-DRiHI73zHgR6phiuyrjDoMgHiMMOiKpkE5u_wxk6UJS7VNhVDQSBTCfu-qFGZw5x-SOb2zlyWjcq7O9DgyuR9Tm-vA_c8iF-tAwKKfUzTMB0Z=w1204-h677-no" alt="Behavior Model Two" align="" height=350>
 <br>
<h3>
<a id="challenges-triumphs" class="anchor" href="#challenges-triumphs" aria-hidden="true"><span class="octicon octicon-link"></span></a>Challenges and Triumphs</h3>
<h4>Challenges</h4>
<p>Data prep was one of our largest challenges - It took some time to understand the MEPS survey, how it was conducted, and what variables we needed to look into. Another challenge we faced was dealing with the imbalanced nature of the dataset. There are many more observations of non-hospitlization than hospilization among survey respondents. We needed to weight hospilization observations more heavily for our model to have better generaliztion accuracy.</p>
<br>
<h4>Triumphs</h4>
<p>Out of 1900 variables we able to isolate specific features for plan design, behaviors, and hospitalizations from the MEPS dataset.
Created a model for predicting hospitalizations from patient behaviors with an accuracy above 88 percent, unprecedented in the industry.
Developed an interactive R-Shiny app to display our results for Benefitfocus staff.
</p>

</div>
</body>
</html>
