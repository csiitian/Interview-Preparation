# Singleton Design Pattern

This is a creation design pattern which ensures that a class has only one instance and provides a global point of access to it.

## Problem
Let's say you have a class which is responsible for creating a connection to a database. 
Now, you don't want to create multiple connections to the database as it will be a waste of resources. 
So, you want to create only one instance of the class which is responsible for creating the connection to the database.

## Solution
The solution is to create a class which has a private constructor and a static method which will return the instance of the class.
So whenever you want to create an instance of the class, you will call the static method of the class which will return the instance of the class.