package com.bytebach.impl;

import java.util.*;

import com.bytebach.model.*;

public class MyDatabase implements Database {
	// This is where you'll probably want to start. You'll need to provide an
	// implementation of Table as well.
	//
	// One of the key challenges in this assignment is to provide you're
	// own implementations of the List interface which can intercept the various
	// operations (e.g. add, set, remove, etc) and check whether they violate
	// the constraints and/or update the database appropriately (e.g. for the
	// cascading delete).
	//
	// HINT: to get started, don't bother providing your own implementations of
	// List as discussed above! Instead, implement MyDatabase and MyTabe using
	// conventional Collections. When you have that working, and the web system
	// is doing something sensible, then consider how you're going to get those
	// unit test to past.

    private Map<String, Table> tables;

    public MyDatabase() {
        this.tables = new HashMap<>();
    }

    @Override
    public Collection<? extends Table> tables() {
        return tables.values();
    }

    @Override
    public Table table(String name) {
        if (!tables.containsKey(name)) {
            throw new InvalidOperation("Table does not exist");
        }
        return tables.get(name);
    }

    @Override
    public void createTable(String name, List<Field> fields) {
        if (tables.containsKey(name)) {
            throw new InvalidOperation("Table already exists");
        }
        tables.put(name, new MyTable(this, name, fields));
    }

    @Override
    public void deleteTable(String name) {
        if (tables.containsKey(name)) {
            // search through all tables that have this table as a ref

            tables.remove(name);
        }
        throw new InvalidOperation("Table must exist to be deleted");
    }
}
