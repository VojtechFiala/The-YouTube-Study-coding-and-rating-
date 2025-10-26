# The-YouTube-Study-coding-and-rating-
The repository contain coded data on 800 YouTube video thumbnails as well as the ratings of their titles relevance. Other data will come as soon as they are collected.

Coded.content.csv This is a list of the video thumbnails, coded for the content by the author of the project. Concretely, we focused on the following: If there is a human body and face in the thumbnail, if the face is shown in closeup, if the human (whenever present) is gesturing and whether some part of the thumbnail content possess characteristics typical for AI (text to image LLM) generated content. Furthermore, we coded if the thumbnail contains text (including in a photo or on a computer desktop), and, if present, whether the text contain/repeats the video title. Lastly, we coded if the thumbnail is of very low quality. 

Duplicities_maybe.csv For one half of the searches (N=400 videos) we repeated the process originally done in Torr browser (which is very incognito with regard to the end user) using standard Chrome browser (which "knows" the user and the YouTube algorithm should accomodate the results to this fact). Nothing like that was, however, observed as the overlap between results provided in both searches overlapped quite extensively. 

Evolang_Submission_Script.R The script that should guide you through all the analyses. Parental advisory - strong/mature language may be present (this is what always happen when I try to programm something). 

Relevant_Irrelevant_5.csv - raw data. Should be perfectly anonymous, though. 

YouTube_study.Rproj - please download and run the script through this. 
