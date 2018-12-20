
Project Presentation
========================================================

<br>

<h2> Swiftkey NLP Assignment</h2>

<h4>    Data Science Capstone Project</h4>
</br>
Inder Chettri   
April, 2016    
   

Overview
========================================================
left: 80%
<font size= 6em>
Entering data into a smartphone screen is hard. And we have developed a Swiftkey like app that uses predictive analytics to guess the next word the user may type.

The next slides will cover the following areas

- Operating Instructions: How the app works
- Algorithm: Katz Back Off or Interpolation
- Optimizations: Increase accuracy and decrease response time.

Check out the app at https://indc.shinyapps.io/Capstone_SwiftkeyNLP_App/   
And try it out using both your desktop and your mobile device!
</font>
***


<font size= 5em>   Mobile View:</font>

![alt text](AppImage3.png)

Operating Instructions
========================================================
left: 45%
![alt text](AppImage0.jpg)
<font size= 5em>Desktop View:</font>
***
<font size= 6em>
Basic Operations   
</font>
<font size= 5em>
- On launching the app, you will see a progress indicator on the top right of the page indicating the loading status. Wait for loading to complete.   
- Enter some text in the input box.
- The app will compute the most probable words and show them in the 'Prediction' section. You can click on any of these words to include them in the initial phrase or you can type a new phrase altogether.   

</font>
<font size= 6em>Advanced Options are explained in the next slide.</font>

Algorithms / Advanced Options
========================================================
left: 25%
![alt text](AppImage2.jpg)
<font size= 5em>Desktop View:</font>
***
Advanced Options    
<font size= 5em>
You can use Advanced Options to change how the app works.

The text data (blogs, news, twitter) was converted into lists of one word (unigram), two (bigram) and three(trigram) along with their frequency counts. This was converted to probability and smoothened using Good-Turing algorithm.

The app used Katz's Back Off method as the default for prediction. The last two words are matched against the first two words in the trigram table and the third word with the highest probability is shown to the user. If there is no trigram match, the process is repeated in bigrams and then the unigram table.

The other option provided is the Interpolated method. This looks up the last word values in unigram,  bigram adn trigram and computes a weighted probability value (weights need to total to 1). The words with teh highest probability is shown to the user.

</font>

Optimizations
========================================================
<font size= 5em>
The output of the model utlizing the full data set was more accurate. However, the model was slow because of the size of the unigram, bigram and trigram files.

The following optimizations were made to increase the responsiveness of the model and reduce the data size.

- Data Stemming and advanced cleaning.
- Indexing - Replacing Words with Numbers.
- Precalculating the probabilities for word occurance.
- Picking only top 5 most frequently occuring bigrams in the trigram table.
- Use data table and keys

Using these optimizations, the complete english data was compressed from around 2 gigabytes to less than 150Mb.
</font>
