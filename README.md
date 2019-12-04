# Reasoning From Memory
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

* NodeJS (>= 12.0.0)
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

Install node modules.

``` sh
npm install
```

## Usage
There are two ways to use this application: In a browser or in a shell. The application may take ~15 seconds to load, since it has to load word embeddings for the English language.

### Browser
Start the server.

``` sh
websocketd --port 10579 python start_web.py
```

Run this command in another terminal session to open the web application:

``` sh
npm start
```

The browser should automatically open a new window/tab, but go to `localhost:8080` in case it does not.

### Shell
Run this command:

``` sh
python start_shell.py
```
