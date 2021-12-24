### An implementaion of Ruby mini-language

This is a homework for functional programming course.

License: LGPL

Author: Ivanov Kirill, ivanov.kirill.a@mail.ru

Features done:

- ast;
- parser;
- pretty printer;
- interpreter;
- stdlib + repl;
- tests.

Замечания:

- поддерживается следующий синтаксис присваиваний: `v0, v1, ... , vn = expr0, ...` ;
- `x = y = z = expr` -- не поддерживается;
- методы без круглых скобок не поддерживаются;
- поддерживаются только строки в двойных кавычках;
- лямбды не поддерживают замыкания;
- вложенные классы и методы не поддерживаются;
- списки -- это питоновские списки, то есть динамические массивы;
- программа исполняется в контексте экземпляра main класса Object (согласно спецификации Ruby);
- ключевое слово `self` при объявлении метода внутри класса эквивалентно объявлению метода класса;
- метод без ассоциированной с ним переменной (`def method ...`) является методом экземпляра класса.