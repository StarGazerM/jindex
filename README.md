# Jindex

--------------------------


Jindex is a web java syntax indexer. Most idea of this little toy come from some other project for example rubysonar/pysonar. 

Basically it is an m-CFA based static analysis tool. It will do pointer analysis to a code snippit and helping a web component to do something like syntax matching, code navigation.

*NOTE: this is just a course project for syracuse's CSE687 course :-)*


## What's here
- browser based frontend demo for symbol index
- a test web server
- java language parser
- static analyizer
- some test program

## some approximation

1. Since our tool is just a course homework level things, so not all Java language feature will be supported here. The
grammar of "core java" is defined in antlr folder most of parser code is stolen from (antlr's example repo)[https://github.com/antlr/grammars-v4/blob/master/java/java8]

## Environment
- oracle jdk 13
- IntelliJ Idea


