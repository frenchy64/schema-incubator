# schema-incubator

Incubator for Plumatic Schema features.

### Polymorphic schemas (com.ambrosebs.schema-incubator.poly)

Macros such as `poly/defn` can define functions with polymorphic schemas. At runtime, they will be checked
by expanding polymorphic variables to their most general values. For example, at runtime `identity-mono`
and `identity-poly` are instrumented in the same way:

```clojure
(poly/defn identity-mono :- s/Any
  [x :- s/Any]
  x)

(poly/defn :all [T]
  identity-poly :- T
  [x :- T]
  x)
```

The actual value chosen as the "most general" depends on the polymorphic variables kind and should not be
relied on. In the future, polymorphic variables may be instantiated with other values.

Dotted variables have an internal "most general" value which represents a homogeneous sequence of
generalized templates (ie., generalizing variables to the left of the `:..`).
The following two functions are instrumented in the same way.

```clojure
(poly/defn :all [S T :..]
  rest-args-poly :- S
  [& xs :- {:a S :b T} :.. T]
  x)

(s/defn rest-args-mono :- s/Any
  [& xs :- [{:a s/Any :b s/Any}]]
  x)
```

## License

Copyright © 2022 Ambrose Bonnaire-Sergeant

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
