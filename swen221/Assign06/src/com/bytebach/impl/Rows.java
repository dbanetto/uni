package com.bytebach.impl;

import com.bytebach.model.*;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import java.util.*;

/**
 * Created by drb on 16/05/15.
 */
public class Rows implements List<List<Value>> {
    private final Database owner;
    private final List<Field> fields;
    private List<List<Value>> values;

    public Rows(Database owner, List<Field> fields) {
        this.owner = owner;
        this.fields = fields;
        values = new ArrayList<>();
    }

    /**
     * Validate references to other tables to be correct
     *
     * @param value list of values to check
     */
    private void validateRefs(List<Value> value) {
        // Test for bad references
        for (Value val : value) {
            if (val instanceof ReferenceValue) {
                ReferenceValue refval = (ReferenceValue) val;
                owner.table(refval.table()).row(refval.keys());
            }
        }
    }

    private void validateDuplicate(List<Value> value, List<Value> skip) {
        // Test for duplicates
        rows: for (List<Value> row : values) {
            if (row == skip) {
                continue;
            }
            int i = 0;
            for (Value val : row) {
                Field field = this.fields.get(i);
                if (field.isKey()) {
                    if (!value.contains(val)) {
                        continue rows;
                    }
                }
                i++;
            }
            throw new InvalidOperation("Duplicate keys");
        }
    }

    @Override
    public boolean add(List<Value> value) {
        validateRefs(value);
        validateDuplicate(value, null);

        this.values.add(value);
        return true;
    }

    @Override
    public boolean remove(Object o) {
        return this.values.remove(o);
    }

    @Override
    public void add(int index, List<Value> element) {
        throw new NotImplementedException();
    }

    @Override
    public int size() {
        return values.size();
    }

    @Override
    public boolean isEmpty() {
        return values.isEmpty();
    }

    @Override
    public boolean contains(Object o) {
        return this.values.contains(o);
    }

    @Override
    public Iterator<List<Value>> iterator() {
        return this.values.iterator();
    }

    @Override
    public Object[] toArray() {
        throw new NotImplementedException();
    }

    @Override
    public <T> T[] toArray(T[] a) {
        throw new NotImplementedException();
    }

    @Override
    public boolean containsAll(Collection<?> c) {
        throw new NotImplementedException();
    }

    @Override
    public boolean addAll(Collection<? extends List<Value>> c) {
        for (List<Value> i : c) {
            if (!this.add(i)) { // FIXME: Not atomic
                return false;
            }
        }
        return true;
    }

    @Override
    public boolean addAll(int index, Collection<? extends List<Value>> c) {
        throw new NotImplementedException();
    }

    @Override
    public boolean removeAll(Collection<?> c) {
        throw new NotImplementedException();
    }

    @Override
    public boolean retainAll(Collection<?> c) {
        throw new NotImplementedException();
    }

    @Override
    public void clear() {
        this.values.clear();
    }

    @Override
    public List<Value> get(int index) {
        return this.values.get(index);
    }

    @Override
    public List<Value> set(int index, List<Value> element) {
        throw new NotImplementedException();
    }

    @Override
    public List<Value> remove(int index) {
        return this.values.remove(index);
    }

    @Override
    public int indexOf(Object o) {
        throw new NotImplementedException();
    }

    @Override
    public int lastIndexOf(Object o) {
        throw new NotImplementedException();
    }

    @Override
    public ListIterator<List<Value>> listIterator() {
        return this.values.listIterator();
    }

    @Override
    public ListIterator<List<Value>> listIterator(int index) {
        return this.values.listIterator(index);
    }

    @Override
    public List<List<Value>> subList(int fromIndex, int toIndex) {
        throw new NotImplementedException();
    }
}
