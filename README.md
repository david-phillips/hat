# ^hat^

### Dead simple HTML templating in Prolog.

Hat templates are a mix of markup & code.

They look like this:

```
my_template('
<html>
  <head>
    <title>^page_title^</title>
  </head>
  <body>
    <table id="books-table" class="hat-template">
        ^make_table_rows^
    </table>
  </body>
</html>
').
```

Code occurs between two hat (aka caret) symbols and can  
only be Prolog predicates.

In order for these template predicates to populate your  
markup you must have matching clauses somewhere in your  
source code.

For instance, you might have,

```
page_title('Favorite Prolog Books').
```

-The matching clauses must have an arity of 1.
-The argument of the clause must expect an unbound variable.

The job of the clause is to bind its argument variable to a  
value that will get placed into the template's markup.

Check out the hat_test_client.pl for a (hopefully) working  
example.
