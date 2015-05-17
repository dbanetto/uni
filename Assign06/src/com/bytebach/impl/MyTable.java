package com.bytebach.impl;

import com.bytebach.model.*;

import java.beans.FeatureDescriptor;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Created by drb on 13/05/15.
 */
public class MyTable implements Table {
    private final Database owner;
    private final String name;
    private final List<Field> fields;
    private List<MyTable> linkedFrom;
    private Rows rows;

    public MyTable(Database owner, String name, List<Field> fields) {
        this.owner = owner;
        this.name = name;
        this.fields = fields;
        this.rows = new Rows(owner, fields);
        this.linkedFrom = new ArrayList<>();

        // Tell the tables this depends on that we want to know of deletes
        for (Field f : fields) {
            if (f.type() == Field.Type.REFERENCE) {
                Table t = owner.table(f.refTable());
                if (t instanceof MyTable) {
                    MyTable mt = (MyTable)t;
                    mt.linkedFrom.add(this);
                }
            }
        }
    }

    @Override
    public String name() {
        return name;
    }

    @Override
    public List<Field> fields() {
        return fields;
    }

    @Override
    public List<List<Value>> rows() {
        return rows;
    }

    @Override
    public List<Value> row(Value... keys) {
        List<Value> keyList = Arrays.asList(keys);
        List<Value> foundRow = null;

        rows: for (List<Value> row : rows) {
            int i = 0;
            for (Value val : row) {
                Field field = this.fields.get(i);
                if (field.isKey()) {
                    if (!keyList.contains(val)) {
                        continue rows;
                    }
                }
                i++;
            }
            foundRow = row;
            break;
        }
        if (foundRow == null) {
            throw new InvalidOperation("Does not exist");
        }
        return foundRow;
    }

    @Override
    public void delete(Value... keys) {
        List<Value> keyList = Arrays.asList(keys);

        List<Value> rowToDelete = null;
        List<Value> keyToCheck = new ArrayList<>();

        rows: for (List<Value> row : rows) {
            int i = 0;
            for (Value val : row) {
                Field field = this.fields.get(i);
                if (field.isKey()) {
                    if (keyList.contains(val)) {
                        keyToCheck.add(val);
                    } else {
                        keyToCheck.clear();
                        continue rows;
                    }
                }
                i++;
            }
            rowToDelete = row;
            break;
        }
        if (rowToDelete == null) {
            throw new InvalidOperation("Does not exist");
        }
        this.rows.remove(rowToDelete);

        if (!linkedFrom.isEmpty()) {
            for (MyTable t : linkedFrom) {
                Value[] vals = new Value[keyList.size()];
                keyToCheck.toArray(vals);
                t.deleteContains(new ReferenceValue(name, vals));
            }
        }
    }

    /**
     * delete any row that contains keys
     *
     * @param keys
     */
    private void deleteContains(Value... keys) {
        List<Value> keyList = Arrays.asList(keys);

        List<List<Value>> toDelete = new ArrayList<>();

        rows: for (List<Value> row : rows) {
            for (Value val : row) {
                if (keyList.contains(val)) {
                    toDelete.add(row);
                    continue rows;
                }
            }
        }

        for (List<Value> vals : toDelete) {
            Value[] toDeleteKeys = new Value[vals.size()];
            vals.toArray(toDeleteKeys);
            this.delete(toDeleteKeys);
        }

    }
}
