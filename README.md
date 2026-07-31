# HT

## A DSL for computer hardware security research

## Introduction

For such a long time, computer hardware security researchers have been constructing their own tools to create collisions for hardware units, which is a very time-consuming and error-prone process. What's worse, once you change one piece of code, the whole system breaks.

This project aims to provide a domain-specific language (DSL) for computer hardware security research, which offers (micro-)architecture-indenpendent abstractions for basic hardware exploit techniques and the ability to explicitly control the memory layout of victim and attacker programs, either data or code. Further, it offers a clear abstraction of victim, attacker and data collection part of the code, allowing an easier way to verify the correctness of PoCs.

## Implementation

Inspired by Chisel, this DSL is built upon scala. Scala has a powerful expressive syntax and a strong type system, which makes it a good choice for building a DSL. The DSL is designed to be a library in scala, which means that you can import it to your own scala project.

