# ARCHIVED 

# Orihime

## To-do list

### Gameplan

Change database schema to support users

Get authentication working

Sanitize input from third-parties

Separate pods mysql, hunchentoot, and orihime pods out; expose them as services with selectors based on environment

Deal with multiple selections

### Adding sentence and then changing it later

How should I deal with updating the notes and cards after editing the sentence?

For example the following sentence. I would like to add 人一倍 to the list of child words.

けれども邪悪に対しては、人一倍に敏感であった。

### Generate the inverse (block word in context and guess from definition/context)

This would be helpful for the following circumstance.

繁殖して頭数が増えていくことが危惧されている。

I want to thing of the word 危惧 in this context, so a card like the following would be helpful

繁殖して頭数が増えていくことが[...]されている。

