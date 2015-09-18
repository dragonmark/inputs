# A ClojureScript library that generates Reagent/React forms

Based on [Om Input](https://github.com/hiram-madelaine/om-inputs)

> You just go down the list of all the most important things on the internet and what you find is :    
> a Form is usually the broker for the stuff that matters  

[Luke Wroblewski from Conversions@Google 2014 - Part 2](https://www.youtube.com/watch?v=nmKMz3Fg76M)  


## Project Goals

* Generate forms with specification using [Prismatic/Schema](https://github.com/Prismatic/schema).
* Provide best practice UX
* Use no external JS Framework dependencies beside React.js
* Unified client/server validation

## Artifacts

[![Clojars Project](http://clojars.org/dragonmark/inputs/latest-version.svg)](http://clojars.org/dragonmark/inputs)

## How does it work

### Anatomy of a component


To build a component we need :
* A name ;
* A description of the fields ;
* A callback function to use the data ;
* Options to customize the component.



#### The component name

The name is used :

* as the React.js display name;
* To differentiate components in the UI.


#### Description of the fields

The fields of a component are described with Schema.

Using Schema allows the :
* Validation of the data ;
* Coercion of String to proper types.

##### Supported atomic Schema types

* s/Str
* s/Int
* s/Inst
* s/enum
* s/Bool
* s/Eq
* s/Regex

A value can be nil using s/maybe :

```
{:person-first-name (s/maybe s/Str)}

```


A key can be optional using s/s/optional-key :
```
 {(s/optional-key :person-size) s/Num}
```

##### Example

```
(def sch-person {:person-first-name s/Str
                 :person-name s/Str
                 (s/optional-key :person-birthdate) s/Inst
                 (s/optional-key :person-size) s/Int
                 (s/optional-key :person-gender) (s/enum "M" "F")})
```

### Build a Reagent input component

To build an input component, just call the function `make-input-comp` with the required parameters :
- A keyword for the component name
- A Prismatic/Schema
- a callback function

In this example we build the component :create-person with the Schema seen previously and the callback simply diplay the created map :

```
(def person-input-view (make-input-comp :create-person sch-person #(js/alert %)))
```


### Translation of the Schema into UI.



#### The form inputs

Each entry of a schema generate a field in the form.

Hence, the example schema will produce a form with these input fields :

* A mandatory input of type text for :person-first-name ;
* A mandatory input of type text for :person-name ;
* An optional date input for the :person-birthdate ;
* An optional input that allows only Integer for :person-size ;
* An optional select that that present the choices "M" and "F" ;
* A validation button that trigger the callback.

#### Fields validation

There are two type of validations :

1.  Schema validation
2.  Verily validation

##### Schema Validation

Schema is able to check that a data structure is conform :

In case of a map :

* all required keys are present ;
* All values are of the correct type ;

This job is done by Schema/Coercion :
When a value is not of the declared type, we have a chance to coerce it in the correct type.


The problem with an HTMl form is that all data are strings.

* An empty string represents nil
* Other types must be coerced to the correct type : s/Num, s/Int, s/Inst



##### When validations occur ?


* A required input must have a non blank value ;
* A coercion appends if needed for type different than s/Str


#### Options

Options are a mean to override the default behavior of the library.

All options are given in a map.


##### Order of fields

The schema is a map that can't be ordered so the fields are displayed in a random order.


You can define the total ordering by giving a vector :

```
(def opts {:order [:person-first-name :person-name :person-gender :person-birthdate :person-size :person-married]})

```


##### Change the rendering (implementation may change)


###### Different way to display an enum

By default the enum is display as a dropdown list
It is possible to choose different representations :
Vertical Group of radio buttons :
```
(def opts {:person-gender {:type "radio-group"}})

```
Horizontal group of radio buttons :

```
(def opts {:person-gender {:type "radio-group-inline"}})

```

Segmented controls :
```
(def opts {:person-gender {:type "btn-group"}})

```

Stepper (-|+) :
```
(def opts {:items/number {:type "stepper"
                          :attrs {:min 0 :max 10 :step 1}}})
```


##### More Complex Validation rules

It is possible to add more complex validation rules than the one provided by Schema.

I chose [Verily](https://github.com/jkk/verily) for the following reasons :

* the rules can be described as data structure
* the rules are expressed on the whole map not by key.
* It works for Clojure and ClojureScript.


######  Add validations rules


```
(def opts {:validations [[:min-val 100 :person-size :person-size-min-length]
                         [:email :person-email :bad-email]]})
```



##### Initial value (not implemented yet)

It should be possible to have initial values for each field.

```
(def opts {:init {:person-married true}})

```

The initial data could be retrieved from the cursor app-state.



## Copyright and license

Copyright © 2013-2015 Hiram Madelaine and David Pollak


Licensed under the EPL (see the file LICENSE.md).
