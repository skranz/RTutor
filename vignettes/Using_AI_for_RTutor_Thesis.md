# Using AI when writing an RTutor BA / MA Thesis 

Sebastian Kranz, Ulm University 

Version from 2026-01-04


A challenging Bachelor or Master thesis is a great opportunity to learn using modern AI tools. You are allowed and encouraged to use AI to improve the quality of your thesis. But you must still be the main person that designs and writes it and understand every aspect of your thesis.

This document gives some tips on using AI.

### 1. Better results with better AI models

Most interfaces like ChatGPT allow a selection of different models. There can be substantial differences and more powerful models or an explicit thinking mode tends to yield better output.

### 2. Sometimes restart conversations

Quality of AI responses can get worse once conversations become very long. If you realize this, try starting a new conversation.

### 3. Context matters - Give the AI all the information it needs

Assume your R code seems to yield different results than shown in the article. There can be different reasons:

- Possibly you made an error when translating the Stata code to R

- The R command may just run differently than Stata

- The authors made an error when copying their Stata results to the article or used a slightly different data set than they provided in the replication package.

AI can often help to figure out the reason for the problem. But AI has little chance to figure out the problem without having all relevant information, i.e. ideally you should provide it with

- Your question / task for the AI
- your R code
- the original Stata code
- the article

Other information that could be helpful

- the results of running your R code
- if available the logs of the Stata run
- the README file of the reproduction package

Note: If you upload copyrighted material to the AI check that this is legal, e.g. often the case only if you turn-off the option that the AI model can be trained on your input.  

Of course uploading all documents takes time, in particular given advice 2 to restart conversations from time to time.

But there are different solutions. Try out what fits best for you. For files that don't (often) change these can be good choices:

- ChatGPT offers projects where you upload documents that work for all chats based on this project

- Google's NotebookLM also allows to upload background documents but to delete the chat history.

I would also generate and a simple txt or md file `info_for_Ai.md` where you give some general background info to the AI. Here you can also add relevant URLs. For example you could add the following text:

```
If I ask you to debug some technical issue with my RTutor problem set,
look at the RTutor Github page and documentation here: 

https://github.com/skranz/RTutor
https://github.com/skranz/RTutor/blob/master/vignettes/02_ElementsSolutionFile.Rmd
```

I would not add your RTutor problem set itself, i.e. the `_sol.Rmd` file to the fixed project files, since it often changes. When a conversation involves your current problem set, I would simply copy & paste the whole text of your `_sol.Rmd` into the prompt.  

For my own R projects, where I have multiple code files that ofthe change, I use my RStudio Addin [https://github.com/skranz/files2prompt](https://github.com/skranz/files2prompt) that allows to quickly combine several text files to a single prompt file that I then copy & paste to the Chat interface. But that probably only makes sense, once you have more than one file that often changes and should be given to the AI.

### 4. For learning things: Try to speak to the AI

AI is not only great to improve the language of your thess, solve coding tasks, or to give general feedback. It is also a great tool for learning: for you to better understand the details of the methods used in the article or to get more economic background. Also here it is useful to use a project with all background information (see advice 3). For me, I get the best interactive learning experience when I then speak to the model, often using the AI app on my mobile phone, instead of typing everything. Simply try out what works best for you. 

### 5. Dangers of AI use

Also good models hallucinate from time to time. Don't simply trust all outputs. E.g. AI can be useful to find related research, but you have to read and check the ressources.

While AI can for many non-native English speakers often substantially their language, it is mostly not yet very good in the overall structure of longer documents. You should be the main designer of your thesis. Ask AI for advice and refine your thesis, but don't let it design everything. In my experience just copying AI output withouth thinking yourself and without knowing all details of your thesis yields poor quality with correspondingly poor grades. 