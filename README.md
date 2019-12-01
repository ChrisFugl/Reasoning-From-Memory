# Reasoning From Memory
**This project is under development.**

This is a proof of concept of an application that lets a user enter facts using natural language, and the application can later use these facts together with first-order logic inference to answer user questions.

For example:
```
User enters: Seoul is in Korea.
User enters: Korea lies in Asia.
User asks: Is Seoul located in Asia?
Systems responds: Yes.
```

In this example the system stores the two entered facts and uses them to infer that Seoul is located in Asia. Notice how both memory and reasoning was necessary to answer the two question.

## Requirements
This work depends on the following requirements:

* Python (>= 3.6)
* SWI-Prolog (>= 8.0.0)
* [websocketd](https://github.com/joewalnes/websocketd)

## Installation
Start by cloning the repository:

``` sh
git clone https://github.com/ChrisFugl/Reasoning-From-Memory
```

Install Python packages through pip:

``` sh
pip install -r requirements.txt
```

Install English core for spaCy.

``` sh
python -m spacy download en_core_web_md
```
