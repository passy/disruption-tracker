# Insertion

This seems to work generally for merging disruptions for named routes:

```
r.table('disruptions').insert([{ id: 'Metropolitan', disruptions: [5, 8] }], {
  conflict: function (id, oldDoc, newDoc) {
    return newDoc.merge({
      'disruptions': oldDoc('disruptions').setUnion(newDoc('disruptions'))
    });
  }
})
```
