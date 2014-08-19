var PS = PS || {};
PS.Prelude = (function () {
    "use strict";
    var Unit = {
        create: function (value) {
            return value;
        }
    };
    function Semigroupoid($less$less$less) {
        this["<<<"] = $less$less$less;
    };
    function Category(__superclass_Prelude$dotSemigroupoid_0, id) {
        this["__superclass_Prelude.Semigroupoid_0"] = __superclass_Prelude$dotSemigroupoid_0;
        this.id = id;
    };
    function Show(show) {
        this.show = show;
    };
    function Functor($less$dollar$greater) {
        this["<$>"] = $less$dollar$greater;
    };
    function Apply($less$times$greater, __superclass_Prelude$dotFunctor_0) {
        this["<*>"] = $less$times$greater;
        this["__superclass_Prelude.Functor_0"] = __superclass_Prelude$dotFunctor_0;
    };
    function Applicative(__superclass_Prelude$dotApply_0, pure) {
        this["__superclass_Prelude.Apply_0"] = __superclass_Prelude$dotApply_0;
        this.pure = pure;
    };
    function Bind($greater$greater$eq, __superclass_Prelude$dotApply_0) {
        this[">>="] = $greater$greater$eq;
        this["__superclass_Prelude.Apply_0"] = __superclass_Prelude$dotApply_0;
    };
    function Monad(__superclass_Prelude$dotApplicative_0, __superclass_Prelude$dotBind_1) {
        this["__superclass_Prelude.Applicative_0"] = __superclass_Prelude$dotApplicative_0;
        this["__superclass_Prelude.Bind_1"] = __superclass_Prelude$dotBind_1;
    };
    function Eq($div$eq, $eq$eq) {
        this["/="] = $div$eq;
        this["=="] = $eq$eq;
    };
    function Semigroup($less$greater) {
        this["<>"] = $less$greater;
    };
    function cons(e) {  return function (l) {    return [e].concat(l);  };};
    function showStringImpl(s) {  return JSON.stringify(s);};
    function showNumberImpl(n) {  return n.toString();};
    function showArrayImpl (f) {  return function (xs) {    var ss = [];    for (var i = 0, l = xs.length; i < l; i++) {      ss[i] = f(xs[i]);    }    return '[' + ss.join(',') + ']';  };};
    function refEq(r1) {  return function(r2) {    return r1 === r2;  };};
    function refIneq(r1) {  return function(r2) {    return r1 !== r2;  };};
    function concatString(s1) {  return function(s2) {    return s1 + s2;  };};
    var $greater$greater$eq = function (dict) {
        return dict[">>="];
    };
    var $less$greater = function (dict) {
        return dict["<>"];
    };
    var $less$times$greater = function (dict) {
        return dict["<*>"];
    };
    var $less$dollar$greater = function (dict) {
        return dict["<$>"];
    };
    var $colon = cons;
    var $div$eq = function (dict) {
        return dict["/="];
    };
    var $plus$plus = function (__dict_Semigroup_1) {
        return $less$greater(__dict_Semigroup_1);
    };
    var $dollar = function (f) {
        return function (x) {
            return f(x);
        };
    };
    var unit = {};
    var showString = function (__unused) {
        return new Show(showStringImpl);
    };
    var showNumber = function (__unused) {
        return new Show(showNumberImpl);
    };
    var show = function (dict) {
        return dict.show;
    };
    var showArray = function (__dict_Show_2) {
        return new Show(showArrayImpl(show(__dict_Show_2)));
    };
    var semigroupoidArr = function (__unused) {
        return new Semigroupoid(function (f) {
            return function (g) {
                return function (x) {
                    return f(g(x));
                };
            };
        });
    };
    var semigroupString = function (__unused) {
        return new Semigroup(concatString);
    };
    var pure = function (dict) {
        return dict.pure;
    };
    var $$return = function (__dict_Monad_4) {
        return pure(__dict_Monad_4["__superclass_Prelude.Applicative_0"]({}));
    };
    var liftA1 = function (__dict_Applicative_6) {
        return function (f) {
            return function (a) {
                return $less$times$greater(__dict_Applicative_6["__superclass_Prelude.Apply_0"]({}))(pure(__dict_Applicative_6)(f))(a);
            };
        };
    };
    var id = function (dict) {
        return dict.id;
    };
    var eqNumber = function (__unused) {
        return new Eq(refIneq, refEq);
    };
    var $$const = function (_25) {
        return function (_26) {
            return _25;
        };
    };
    var categoryArr = function (__unused) {
        return new Category(semigroupoidArr, function (x) {
            return x;
        });
    };
    var ap = function (__dict_Monad_14) {
        return function (f) {
            return function (a) {
                return $greater$greater$eq(__dict_Monad_14["__superclass_Prelude.Bind_1"]({}))(f)(function (_2) {
                    return $greater$greater$eq(__dict_Monad_14["__superclass_Prelude.Bind_1"]({}))(a)(function (_1) {
                        return $$return(__dict_Monad_14)(_2(_1));
                    });
                });
            };
        };
    };
    return {
        Unit: Unit, 
        Semigroup: Semigroup, 
        Eq: Eq, 
        Monad: Monad, 
        Bind: Bind, 
        Applicative: Applicative, 
        Apply: Apply, 
        Functor: Functor, 
        Show: Show, 
        Category: Category, 
        Semigroupoid: Semigroupoid, 
        unit: unit, 
        "++": $plus$plus, 
        "<>": $less$greater, 
        refIneq: refIneq, 
        refEq: refEq, 
        "/=": $div$eq, 
        ap: ap, 
        "return": $$return, 
        ">>=": $greater$greater$eq, 
        liftA1: liftA1, 
        pure: pure, 
        "<*>": $less$times$greater, 
        "<$>": $less$dollar$greater, 
        show: show, 
        cons: cons, 
        ":": $colon, 
        "$": $dollar, 
        id: id, 
        "const": $$const, 
        semigroupoidArr: semigroupoidArr, 
        categoryArr: categoryArr, 
        showString: showString, 
        showNumber: showNumber, 
        showArray: showArray, 
        eqNumber: eqNumber, 
        semigroupString: semigroupString
    };
})();
var PS = PS || {};
PS.Data_Validation = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    function Invalid(value0) {
        this.value0 = value0;
    };
    Invalid.create = function (value0) {
        return new Invalid(value0);
    };
    function Valid(value0) {
        this.value0 = value0;
    };
    Valid.create = function (value0) {
        return new Valid(value0);
    };
    var runV = function (_46) {
        return function (_47) {
            return function (_48) {
                if (_48 instanceof Invalid) {
                    return _46(_48.value0);
                };
                if (_48 instanceof Valid) {
                    return _47(_48.value0);
                };
                throw new Error("Failed pattern match");
            };
        };
    };
    var invalid = Invalid.create;
    var functorV = function (__unused) {
        return new Prelude.Functor(function (_51) {
            return function (_52) {
                if (_52 instanceof Invalid) {
                    return new Invalid(_52.value0);
                };
                if (_52 instanceof Valid) {
                    return new Valid(_51(_52.value0));
                };
                throw new Error("Failed pattern match");
            };
        });
    };
    var applyV = function (__dict_Semigroup_17) {
        return new Prelude.Apply(function (_53) {
            return function (_54) {
                if (_53 instanceof Invalid && _54 instanceof Invalid) {
                    return new Invalid(Prelude["<>"](__dict_Semigroup_17)(_53.value0)(_54.value0));
                };
                if (_53 instanceof Invalid) {
                    return new Invalid(_53.value0);
                };
                if (_54 instanceof Invalid) {
                    return new Invalid(_54.value0);
                };
                if (_53 instanceof Valid && _54 instanceof Valid) {
                    return new Valid(_53.value0(_54.value0));
                };
                throw new Error("Failed pattern match");
            };
        }, functorV);
    };
    var applicativeV = function (__dict_Semigroup_18) {
        return new Prelude.Applicative(function (__unused) {
            return applyV(__dict_Semigroup_18);
        }, Valid.create);
    };
    return {
        runV: runV, 
        invalid: invalid, 
        functorV: functorV, 
        applyV: applyV, 
        applicativeV: applicativeV
    };
})();
var PS = PS || {};
PS.Data_String = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    function length(s) {  return s.length;};
    return {
        length: length
    };
})();
var PS = PS || {};
PS.Data_String_Regex = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    function regex$prime(s1) {  return function(s2) {    return new RegExp(s1, s2);  };};
    function source(r) {  return r.source;};
    function flags(r) {  return {    multiline: r.multiline,    ignoreCase: r.ignoreCase,    global: r.global,    sticky: !!r.sticky,    unicode: !!r.unicode  };};
    function test(r) {  return function (s) {    return r.test(s);  };};
    var renderFlags = function (flags_1) {
        return (flags_1.global ? "g" : "") + (flags_1.ignoreCase ? "i" : "") + (flags_1.multiline ? "m" : "") + (flags_1.sticky ? "y" : "") + (flags_1.unicode ? "u" : "");
    };
    var regex = function (source_1) {
        return function (flags_1) {
            return regex$prime(source_1)(renderFlags(flags_1));
        };
    };
    return {
        test: test, 
        renderFlags: renderFlags, 
        flags: flags, 
        source: source, 
        regex: regex
    };
})();
var PS = PS || {};
PS.Data_Function = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    function runFn2(fn) {  return function(a) {    return function(b) {      return fn(a, b);    };  };};
    function runFn3(fn) {  return function(a) {    return function(b) {      return function(c) {        return fn(a, b, c);      };    };  };};
    function runFn4(fn) {  return function(a) {    return function(b) {      return function(c) {        return function(d) {          return fn(a, b, c, d);        };      };    };  };};
    return {
        runFn4: runFn4, 
        runFn3: runFn3, 
        runFn2: runFn2
    };
})();
var PS = PS || {};
PS.Data_Either = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    function Left(value0) {
        this.value0 = value0;
    };
    Left.create = function (value0) {
        return new Left(value0);
    };
    function Right(value0) {
        this.value0 = value0;
    };
    Right.create = function (value0) {
        return new Right(value0);
    };
    var functorEither = function (__unused) {
        return new Prelude.Functor(function (_63) {
            return function (_64) {
                if (_64 instanceof Left) {
                    return new Left(_64.value0);
                };
                if (_64 instanceof Right) {
                    return new Right(_63(_64.value0));
                };
                throw new Error("Failed pattern match");
            };
        });
    };
    var either = function (_60) {
        return function (_61) {
            return function (_62) {
                if (_62 instanceof Left) {
                    return _60(_62.value0);
                };
                if (_62 instanceof Right) {
                    return _61(_62.value0);
                };
                throw new Error("Failed pattern match");
            };
        };
    };
    var applyEither = function (__unused) {
        return new Prelude.Apply(function (_65) {
            return function (_66) {
                if (_65 instanceof Left) {
                    return new Left(_65.value0);
                };
                if (_65 instanceof Right) {
                    return Prelude["<$>"](functorEither({}))(_65.value0)(_66);
                };
                throw new Error("Failed pattern match");
            };
        }, functorEither);
    };
    var bindEither = function (__unused) {
        return new Prelude.Bind(either(function (e) {
            return function (_) {
                return new Left(e);
            };
        })(function (a) {
            return function (f) {
                return f(a);
            };
        }), applyEither);
    };
    return {
        Left: Left, 
        Right: Right, 
        either: either, 
        functorEither: functorEither, 
        applyEither: applyEither, 
        bindEither: bindEither
    };
})();
var PS = PS || {};
PS.Data_AddressBook = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    function HomePhone() {

    };
    HomePhone.value = new HomePhone();
    function WorkPhone() {

    };
    WorkPhone.value = new WorkPhone();
    function CellPhone() {

    };
    CellPhone.value = new CellPhone();
    function OtherPhone() {

    };
    OtherPhone.value = new OtherPhone();
    var PhoneNumber = {
        create: function (value) {
            return value;
        }
    };
    var Address = {
        create: function (value) {
            return value;
        }
    };
    var Person = {
        create: function (value) {
            return value;
        }
    };
    var showPhoneType = function (__unused) {
        return new Prelude.Show(function (_73) {
            if (_73 instanceof HomePhone) {
                return "HomePhone";
            };
            if (_73 instanceof WorkPhone) {
                return "WorkPhone";
            };
            if (_73 instanceof CellPhone) {
                return "CellPhone";
            };
            if (_73 instanceof OtherPhone) {
                return "OtherPhone";
            };
            throw new Error("Failed pattern match");
        });
    };
    var showPhoneNumber = function (__unused) {
        return new Prelude.Show(function (_74) {
            return "PhoneNumber " + "{ type: " + Prelude.show(showPhoneType({}))(_74.type) + ", number: " + Prelude.show(Prelude.showString({}))(_74.number) + " }";
        });
    };
    var showAddress = function (__unused) {
        return new Prelude.Show(function (_72) {
            return "Address " + "{ street: " + Prelude.show(Prelude.showString({}))(_72.street) + ", city: " + Prelude.show(Prelude.showString({}))(_72.city) + ", state: " + Prelude.show(Prelude.showString({}))(_72.state) + " }";
        });
    };
    var showPerson = function (__unused) {
        return new Prelude.Show(function (_75) {
            return "Person " + "{ firstName: " + Prelude.show(Prelude.showString({}))(_75.firstName) + ", lastName: " + Prelude.show(Prelude.showString({}))(_75.lastName) + ", address: " + Prelude.show(showAddress({}))(_75.address) + ", phones: " + Prelude.show(Prelude.showArray(showPhoneNumber({})))(_75.phones) + " }";
        });
    };
    var phoneNumber = function (ty) {
        return function (number) {
            return {
                type: ty, 
                number: number
            };
        };
    };
    var address = function (street) {
        return function (city) {
            return function (state) {
                return {
                    street: street, 
                    city: city, 
                    state: state
                };
            };
        };
    };
    var person = function (firstName) {
        return function (lastName) {
            return function (address_1) {
                return function (phones) {
                    return {
                        firstName: firstName, 
                        lastName: lastName, 
                        address: address_1, 
                        phones: phones
                    };
                };
            };
        };
    };
    return {
        Person: Person, 
        PhoneNumber: PhoneNumber, 
        HomePhone: HomePhone, 
        WorkPhone: WorkPhone, 
        CellPhone: CellPhone, 
        OtherPhone: OtherPhone, 
        Address: Address, 
        person: person, 
        phoneNumber: phoneNumber, 
        address: address, 
        showAddress: showAddress, 
        showPhoneType: showPhoneType, 
        showPhoneNumber: showPhoneNumber, 
        showPerson: showPerson
    };
})();
var PS = PS || {};
PS.Control_Monad_Eff = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    function returnE(a) {  return function() {    return a;  };};
    function bindE(a) {  return function(f) {    return function() {      return f(a())();    };  };};
    function foreachE(as) {  return function(f) {    return function() {      for (var i = 0; i < as.length; i++) {        f(as[i])();      }    };  };};
    var applicativeEff = function (__unused) {
        return new Prelude.Applicative(applyEff, returnE);
    };
    var applyEff = function (__unused) {
        return new Prelude.Apply(Prelude.ap(monadEff({})), functorEff);
    };
    var monadEff = function (__unused) {
        return new Prelude.Monad(applicativeEff, bindEff);
    };
    var bindEff = function (__unused) {
        return new Prelude.Bind(bindE, applyEff);
    };
    var functorEff = function (__unused) {
        return new Prelude.Functor(Prelude.liftA1(applicativeEff({})));
    };
    return {
        foreachE: foreachE, 
        bindE: bindE, 
        returnE: returnE, 
        functorEff: functorEff, 
        applyEff: applyEff, 
        applicativeEff: applicativeEff, 
        bindEff: bindEff, 
        monadEff: monadEff
    };
})();
var PS = PS || {};
PS.Control_Monad_Eff_Storage = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    function setItem(key) {  return function(value) {    return function() {      window.localStorage.setItem(key, value);    };  };};
    function getItem(key) {  return function() {    return window.localStorage.getItem(key);  }};
    return {
        getItem: getItem, 
        setItem: setItem
    };
})();
var PS = PS || {};
PS.Debug_Trace = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    function trace(s) {  return function() {    console.log(s);    return {};  };};
    var print = function (__dict_Show_25) {
        return function (o) {
            return trace(Prelude.show(__dict_Show_25)(o));
        };
    };
    return {
        print: print, 
        trace: trace
    };
})();
var PS = PS || {};
PS.Control_Apply = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var $times$greater = function (__dict_Apply_39) {
        return function (a) {
            return function (b) {
                return Prelude["<*>"](__dict_Apply_39)(Prelude["<$>"](__dict_Apply_39["__superclass_Prelude.Functor_0"]({}))(Prelude["const"](Prelude.id(Prelude.categoryArr({}))))(a))(b);
            };
        };
    };
    return {
        "*>": $times$greater
    };
})();
var PS = PS || {};
PS.Data_Maybe = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    function Nothing() {

    };
    Nothing.value = new Nothing();
    function Just(value0) {
        this.value0 = value0;
    };
    Just.create = function (value0) {
        return new Just(value0);
    };
    return {
        Nothing: Nothing, 
        Just: Just
    };
})();
var PS = PS || {};
PS.Data_Array = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    function length (xs) {  return xs.length;};
    function append (l1) {  return function (l2) {    return l1.concat(l2);  };};
    function map (f) {  return function (arr) {    var l = arr.length;    var result = new Array(l);    for (var i = 0; i < l; i++) {      result[i] = f(arr[i]);    }    return result;  };};
    var semigroupArray = function (__unused) {
        return new Prelude.Semigroup(append);
    };
    var functorArray = function (__unused) {
        return new Prelude.Functor(map);
    };
    return {
        append: append, 
        length: length, 
        map: map, 
        functorArray: functorArray, 
        semigroupArray: semigroupArray
    };
})();
var PS = PS || {};
PS.Data_Monoid = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var mempty = function (dict) {
        return dict.mempty;
    };
    return {
        mempty: mempty
    };
})();
var PS = PS || {};
PS.Data_Foldable = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Data_Monoid = PS.Data_Monoid;
    function Foldable(foldMap, foldl, foldr) {
        this.foldMap = foldMap;
        this.foldl = foldl;
        this.foldr = foldr;
    };
    function foldrArray(f) {  return function(z) {    return function(xs) {      var acc = z;      for (var i = xs.length - 1; i >= 0; --i) {        acc = f(xs[i])(acc);      }      return acc;    }  }};
    function foldlArray(f) {  return function(z) {    return function(xs) {      var acc = z;      for (var i = 0, len = xs.length; i < len; ++i) {        acc = f(acc)(xs[i]);      }      return acc;    }  }};
    var foldr = function (dict) {
        return dict.foldr;
    };
    var foldableArray = function (__unused) {
        return new Foldable(function (__dict_Monoid_101) {
            return function (f) {
                return function (xs) {
                    return foldr(foldableArray({}))(function (x) {
                        return function (acc) {
                            return Prelude["<>"](__dict_Monoid_101["__superclass_Prelude.Semigroup_0"]({}))(f(x))(acc);
                        };
                    })(Data_Monoid.mempty(__dict_Monoid_101))(xs);
                };
            };
        }, function (f) {
            return function (z) {
                return function (xs) {
                    return foldlArray(f)(z)(xs);
                };
            };
        }, function (f) {
            return function (z) {
                return function (xs) {
                    return foldrArray(f)(z)(xs);
                };
            };
        });
    };
    return {
        Foldable: Foldable, 
        foldlArray: foldlArray, 
        foldrArray: foldrArray, 
        foldr: foldr, 
        foldableArray: foldableArray
    };
})();
var PS = PS || {};
PS.Data_Traversable = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Data_Array = PS.Data_Array;
    var Data_Foldable = PS.Data_Foldable;
    function Traversable(__superclass_Data$dotFoldable$dotFoldable_1, __superclass_Prelude$dotFunctor_0, sequence, traverse) {
        this["__superclass_Data.Foldable.Foldable_1"] = __superclass_Data$dotFoldable$dotFoldable_1;
        this["__superclass_Prelude.Functor_0"] = __superclass_Prelude$dotFunctor_0;
        this.sequence = sequence;
        this.traverse = traverse;
    };
    var traverse = function (dict) {
        return dict.traverse;
    };
    var sequence = function (dict) {
        return dict.sequence;
    };
    var traversableArray = function (__unused) {
        return new Traversable(function (__unused) {
            return Data_Foldable.foldableArray({});
        }, function (__unused) {
            return Data_Array.functorArray({});
        }, function (__dict_Applicative_126) {
            return function (_247) {
                if (_247.length === 0) {
                    return Prelude.pure(__dict_Applicative_126)([  ]);
                };
                if (_247.length > 0) {
                    var _323 = _247.slice(1);
                    return Prelude["<*>"](__dict_Applicative_126["__superclass_Prelude.Apply_0"]({}))(Prelude["<$>"]((__dict_Applicative_126["__superclass_Prelude.Apply_0"]({}))["__superclass_Prelude.Functor_0"]({}))(Prelude[":"])(_247[0]))(sequence(traversableArray({}))(__dict_Applicative_126)(_323));
                };
                throw new Error("Failed pattern match");
            };
        }, function (__dict_Applicative_125) {
            return function (_245) {
                return function (_246) {
                    if (_246.length === 0) {
                        return Prelude.pure(__dict_Applicative_125)([  ]);
                    };
                    if (_246.length > 0) {
                        var _327 = _246.slice(1);
                        return Prelude["<*>"](__dict_Applicative_125["__superclass_Prelude.Apply_0"]({}))(Prelude["<$>"]((__dict_Applicative_125["__superclass_Prelude.Apply_0"]({}))["__superclass_Prelude.Functor_0"]({}))(Prelude[":"])(_245(_246[0])))(traverse(traversableArray({}))(__dict_Applicative_125)(_245)(_327));
                    };
                    throw new Error("Failed pattern match");
                };
            };
        });
    };
    return {
        Traversable: Traversable, 
        sequence: sequence, 
        traverse: traverse, 
        traversableArray: traversableArray
    };
})();
var PS = PS || {};
PS.Data_AddressBook_Validation = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Data_String_Regex = PS.Data_String_Regex;
    var Data_Validation = PS.Data_Validation;
    var Data_Array = PS.Data_Array;
    var Data_AddressBook = PS.Data_AddressBook;
    var Control_Apply = PS.Control_Apply;
    var Data_String = PS.Data_String;
    var Data_Traversable = PS.Data_Traversable;
    var Data_Either = PS.Data_Either;
    var phoneNumberRegex = Data_String_Regex.regex("^\\d{3}-\\d{3}-\\d{4}$")({
        unicode: false, 
        sticky: false, 
        multiline: false, 
        ignoreCase: false, 
        global: false
    });
    var nonEmpty = function (_260) {
        return function (_261) {
            if (_261 === "") {
                return Data_Validation.invalid([ "Field '" + _260 + "' cannot be empty" ]);
            };
            return Prelude.pure(Data_Validation.applicativeV(Data_Array.semigroupArray({})))(Prelude.unit);
        };
    };
    var matches = function (_267) {
        return function (_268) {
            return function (_269) {
                if (Data_String_Regex.test(_268)(_269)) {
                    return Prelude.pure(Data_Validation.applicativeV(Data_Array.semigroupArray({})))(Prelude.unit);
                };
                return Data_Validation.invalid([ "Field '" + _267 + "' did not match the required format" ]);
            };
        };
    };
    var validatePhoneNumber = function (_271) {
        return Prelude["<*>"](Data_Validation.applyV(Data_Array.semigroupArray({})))(Prelude["<$>"](Data_Validation.functorV({}))(Data_AddressBook.phoneNumber)(Prelude.pure(Data_Validation.applicativeV(Data_Array.semigroupArray({})))(_271.type)))(Control_Apply["*>"](Data_Validation.applyV(Data_Array.semigroupArray({})))(matches("Number")(phoneNumberRegex)(_271.number))(Prelude.pure(Data_Validation.applicativeV(Data_Array.semigroupArray({})))(_271.number)));
    };
    var lengthIs = function (_264) {
        return function (_265) {
            return function (_266) {
                if (Data_String.length(_266) !== _265) {
                    return Data_Validation.invalid([ "Field '" + _264 + "' must have length " + Prelude.show(Prelude.showNumber({}))(_265) ]);
                };
                return Prelude.pure(Data_Validation.applicativeV(Data_Array.semigroupArray({})))(Prelude.unit);
            };
        };
    };
    var validateAddress = function (_270) {
        return Prelude["<*>"](Data_Validation.applyV(Data_Array.semigroupArray({})))(Prelude["<*>"](Data_Validation.applyV(Data_Array.semigroupArray({})))(Prelude["<$>"](Data_Validation.functorV({}))(Data_AddressBook.address)(Control_Apply["*>"](Data_Validation.applyV(Data_Array.semigroupArray({})))(nonEmpty("Street")(_270.street))(Prelude.pure(Data_Validation.applicativeV(Data_Array.semigroupArray({})))(_270.street))))(Control_Apply["*>"](Data_Validation.applyV(Data_Array.semigroupArray({})))(nonEmpty("City")(_270.city))(Prelude.pure(Data_Validation.applicativeV(Data_Array.semigroupArray({})))(_270.city))))(Control_Apply["*>"](Data_Validation.applyV(Data_Array.semigroupArray({})))(lengthIs("State")(2)(_270.state))(Prelude.pure(Data_Validation.applicativeV(Data_Array.semigroupArray({})))(_270.state)));
    };
    var arrayNonEmpty = function (_262) {
        return function (_263) {
            if (_263.length === 0) {
                return Data_Validation.invalid([ "Field '" + _262 + "' must contain at least one value" ]);
            };
            return Prelude.pure(Data_Validation.applicativeV(Data_Array.semigroupArray({})))(Prelude.unit);
        };
    };
    var validatePerson = function (_272) {
        return Prelude["<*>"](Data_Validation.applyV(Data_Array.semigroupArray({})))(Prelude["<*>"](Data_Validation.applyV(Data_Array.semigroupArray({})))(Prelude["<*>"](Data_Validation.applyV(Data_Array.semigroupArray({})))(Prelude["<$>"](Data_Validation.functorV({}))(Data_AddressBook.person)(Control_Apply["*>"](Data_Validation.applyV(Data_Array.semigroupArray({})))(nonEmpty("First Name")(_272.firstName))(Prelude.pure(Data_Validation.applicativeV(Data_Array.semigroupArray({})))(_272.firstName))))(Control_Apply["*>"](Data_Validation.applyV(Data_Array.semigroupArray({})))(nonEmpty("Last Name")(_272.lastName))(Prelude.pure(Data_Validation.applicativeV(Data_Array.semigroupArray({})))(_272.lastName))))(validateAddress(_272.address)))(Control_Apply["*>"](Data_Validation.applyV(Data_Array.semigroupArray({})))(arrayNonEmpty("Phone Numbers")(_272.phones))(Data_Traversable.traverse(Data_Traversable.traversableArray({}))(Data_Validation.applicativeV(Data_Array.semigroupArray({})))(validatePhoneNumber)(_272.phones)));
    };
    var validatePerson$prime = function (p) {
        return Data_Validation.runV(Data_Either.Left.create)(Data_Either.Right.create)(validatePerson(p));
    };
    return {
        "validatePerson'": validatePerson$prime, 
        validatePerson: validatePerson, 
        validatePhoneNumber: validatePhoneNumber, 
        validateAddress: validateAddress, 
        matches: matches, 
        phoneNumberRegex: phoneNumberRegex, 
        lengthIs: lengthIs, 
        arrayNonEmpty: arrayNonEmpty, 
        nonEmpty: nonEmpty
    };
})();
var PS = PS || {};
PS.Data_Foreign = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Data_Function = PS.Data_Function;
    var Data_Either = PS.Data_Either;
    function ForeignParser(value0) {
        this.value0 = value0;
    };
    ForeignParser.create = function (value0) {
        return new ForeignParser(value0);
    };
    function ReadForeign(read) {
        this.read = read;
    };
    function fromStringImpl(left, right, str) {   try {     return right(JSON.parse(str));   } catch (e) {     return left(e.toString());   } };
    function readPrimTypeImpl(left, right, typeName, value) {   if (toString.call(value) == '[object ' + typeName + ']') {     return right(value);  }   return left('Value is not a ' + typeName + ''); };
    function readPropImpl(k, obj) {     return obj == undefined ? undefined : obj[k];};
    var readPrimType = function (ty) {
        return function (x) {
            return readPrimTypeImpl(Data_Either.Left.create, Data_Either.Right.create, ty, x);
        };
    };
    var readString = function (__unused) {
        return new ReadForeign(ForeignParser.create(readPrimType("String")));
    };
    var read = function (dict) {
        return dict.read;
    };
    var parseForeign = function (_273) {
        return function (_274) {
            return _273.value0(_274);
        };
    };
    var functorForeignParser = function (__unused) {
        return new Prelude.Functor(function (_275) {
            return function (_276) {
                return new ForeignParser(function (x) {
                    return Prelude["<$>"](Data_Either.functorEither({}))(_275)(_276.value0(x));
                });
            };
        });
    };
    var fromString = function (s) {
        return fromStringImpl(Data_Either.Left.create, Data_Either.Right.create, s);
    };
    var parseJSON = function (__dict_ReadForeign_134) {
        return function (json) {
            return Prelude[">>="](Data_Either.bindEither({}))(fromString(json))(parseForeign(read(__dict_ReadForeign_134)));
        };
    };
    var applyForeignParser = function (__unused) {
        return new Prelude.Apply(function (_279) {
            return function (_280) {
                return new ForeignParser(function (x) {
                    var _349 = _279.value0(x);
                    if (_349 instanceof Data_Either.Left) {
                        return new Data_Either.Left(_349.value0);
                    };
                    if (_349 instanceof Data_Either.Right) {
                        return Prelude["<$>"](Data_Either.functorEither({}))(_349.value0)(_280.value0(x));
                    };
                    throw new Error("Failed pattern match");
                });
            };
        }, functorForeignParser);
    };
    var bindForeignParser = function (__unused) {
        return new Prelude.Bind(function (_277) {
            return function (_278) {
                return new ForeignParser(function (x) {
                    var _356 = _277.value0(x);
                    if (_356 instanceof Data_Either.Left) {
                        return new Data_Either.Left(_356.value0);
                    };
                    if (_356 instanceof Data_Either.Right) {
                        return parseForeign(_278(_356.value0))(x);
                    };
                    throw new Error("Failed pattern match");
                });
            };
        }, applyForeignParser);
    };
    var prop = function (__dict_ReadForeign_130) {
        return function (p) {
            return Prelude[">>="](bindForeignParser({}))(new ForeignParser(function (x) {
                return Data_Either.Right.create(readPropImpl$prime(p)(x));
            }))(function (x) {
                return new ForeignParser(function (_) {
                    var _360 = parseForeign(read(__dict_ReadForeign_130))(x);
                    if (_360 instanceof Data_Either.Right) {
                        return new Data_Either.Right(_360.value0);
                    };
                    if (_360 instanceof Data_Either.Left) {
                        return Data_Either.Left.create("Error reading property '" + p + "':\n" + _360.value0);
                    };
                    throw new Error("Failed pattern match");
                });
            });
        };
    };
    var readPropImpl$prime = function (prop_1) {
        return function (x) {
            return readPropImpl(prop_1, x);
        };
    };
    var applicativeForeignParser = function (__unused) {
        return new Prelude.Applicative(applyForeignParser, function (x) {
            return new ForeignParser(function (_) {
                return new Data_Either.Right(x);
            });
        });
    };
    var monadForeignParser = function (__unused) {
        return new Prelude.Monad(applicativeForeignParser, bindForeignParser);
    };
    return {
        ForeignParser: ForeignParser, 
        ReadForeign: ReadForeign, 
        prop: prop, 
        read: read, 
        parseJSON: parseJSON, 
        parseForeign: parseForeign, 
        functorForeignParser: functorForeignParser, 
        bindForeignParser: bindForeignParser, 
        applyForeignParser: applyForeignParser, 
        applicativeForeignParser: applicativeForeignParser, 
        monadForeignParser: monadForeignParser, 
        readString: readString
    };
})();
var PS = PS || {};
PS.Control_Monad_Eff_DOM = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Data_Function = PS.Data_Function;
    var Data_Maybe = PS.Data_Maybe;
    function body() {  return document.body;};
    function createElement(name) {  return function() {    return document.createElement(name);  };};
    function querySelectorImpl(r, f, s) {  return function() {    var result = document.querySelector(s);    return result ? f(result) : r;  };};
    function appendChild(child) {  return function(node) {    return function() {      node.appendChild(child);      return node;    };  };};
    function addClass(className) {  return function(node) {    return function() {      node.classList.add(className);      return node;    };  };};
    function setText(text) {  return function(node) {    return function() {      node.textContent = text;      return node;    };  };};
    function getValue(node) {  return function() {    return node.value;  };};
    function setValue(value) {  return function(node) {    return function() {      node.value = value;      return node;    };  };};
    function setInnerHTML(html) {  return function(node) {    return function() {      node.innerHTML = html;      return node;    };  };};
    function addEventListener(name) {  return function(handler) {    return function(node) {      return function() {        node.addEventListener(name, function(e) {          handler();          e.preventDefault();        });      };    };  };};
    var querySelector = function (s) {
        return querySelectorImpl(Data_Maybe.Nothing.value, Data_Maybe.Just.create, s);
    };
    return {
        addEventListener: addEventListener, 
        setInnerHTML: setInnerHTML, 
        setValue: setValue, 
        getValue: getValue, 
        setText: setText, 
        addClass: addClass, 
        appendChild: appendChild, 
        querySelector: querySelector, 
        querySelectorImpl: querySelectorImpl, 
        createElement: createElement, 
        body: body
    };
})();
var PS = PS || {};
PS.Data_AddressBook_UI = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Control_Monad_Eff = PS.Control_Monad_Eff;
    var Control_Monad_Eff_DOM = PS.Control_Monad_Eff_DOM;
    var Data_Maybe = PS.Data_Maybe;
    var Data_Foreign = PS.Data_Foreign;
    var Data_Either = PS.Data_Either;
    var Debug_Trace = PS.Debug_Trace;
    var Data_AddressBook = PS.Data_AddressBook;
    var Data_Traversable = PS.Data_Traversable;
    var Data_AddressBook_Validation = PS.Data_AddressBook_Validation;
    var valueOf = function (sel) {
        return function __do() {
            var _6 = Control_Monad_Eff_DOM.querySelector(sel)();
            return (function () {
                if (_6 instanceof Data_Maybe.Nothing) {
                    return Prelude["return"](Control_Monad_Eff.monadEff({}))("");
                };
                if (_6 instanceof Data_Maybe.Just) {
                    return function __do() {
                        var _5 = Control_Monad_Eff_DOM.getValue(_6.value0)();
                        var _366 = Data_Foreign.parseForeign(Data_Foreign.read(Data_Foreign.readString({})))(_5);
                        if (_366 instanceof Data_Either.Right) {
                            return _366.value0;
                        };
                        return "";
                    };
                };
                throw new Error("Failed pattern match");
            })()();
        };
    };
    var validateControls = function __do() {
        Debug_Trace.trace("Running validators")();
        var _11 = Prelude["<*>"](Control_Monad_Eff.applyEff({}))(Prelude["<*>"](Control_Monad_Eff.applyEff({}))(Prelude["<*>"](Control_Monad_Eff.applyEff({}))(Prelude["<$>"](Control_Monad_Eff.functorEff({}))(Data_AddressBook.person)(valueOf("#inputFirstName")))(valueOf("#inputLastName")))(Prelude["<*>"](Control_Monad_Eff.applyEff({}))(Prelude["<*>"](Control_Monad_Eff.applyEff({}))(Prelude["<$>"](Control_Monad_Eff.functorEff({}))(Data_AddressBook.address)(valueOf("#inputStreet")))(valueOf("#inputCity")))(valueOf("#inputState"))))(Data_Traversable.sequence(Data_Traversable.traversableArray({}))(Control_Monad_Eff.applicativeEff({}))([ Prelude["<$>"](Control_Monad_Eff.functorEff({}))(Data_AddressBook.phoneNumber(Data_AddressBook.HomePhone.value))(valueOf("#inputHomePhone")), Prelude["<$>"](Control_Monad_Eff.functorEff({}))(Data_AddressBook.phoneNumber(Data_AddressBook.CellPhone.value))(valueOf("#inputCellPhone")) ]))();
        return Data_AddressBook_Validation["validatePerson'"](_11);
    };
    var displayValidationErrors = function (errs) {
        return function __do() {
            var _10 = Prelude[">>="](Control_Monad_Eff.bindEff({}))(Prelude[">>="](Control_Monad_Eff.bindEff({}))(Control_Monad_Eff_DOM.createElement("div"))(Control_Monad_Eff_DOM.addClass("alert")))(Control_Monad_Eff_DOM.addClass("alert-danger"))();
            var _9 = Control_Monad_Eff_DOM.createElement("ul")();
            Control_Monad_Eff_DOM.appendChild(_9)(_10)();
            Control_Monad_Eff.foreachE(errs)(function (err) {
                return function __do() {
                    var _7 = Prelude[">>="](Control_Monad_Eff.bindEff({}))(Control_Monad_Eff_DOM.createElement("li"))(Control_Monad_Eff_DOM.setText(err))();
                    Control_Monad_Eff_DOM.appendChild(_7)(_9)();
                    return Prelude.unit;
                };
            })();
            return Prelude[">>="](Control_Monad_Eff.bindEff({}))(Control_Monad_Eff_DOM.querySelector("#validationErrors"))(function (_8) {
                if (_8 instanceof Data_Maybe.Just) {
                    return function __do() {
                        Control_Monad_Eff_DOM.appendChild(_10)(_8.value0)();
                        return Prelude.unit;
                    };
                };
                throw new Error("Failed pattern match");
            })();
        };
    };
    var validateAndUpdateUI = Prelude[">>="](Control_Monad_Eff.bindEff({}))(Control_Monad_Eff_DOM.querySelector("#validationErrors"))(function (_13) {
        if (_13 instanceof Data_Maybe.Just) {
            return function __do() {
                Control_Monad_Eff_DOM.setInnerHTML("")(_13.value0)();
                var _12 = validateControls();
                (function () {
                    if (_12 instanceof Data_Either.Left) {
                        return displayValidationErrors(_12.value0);
                    };
                    if (_12 instanceof Data_Either.Right) {
                        return Debug_Trace.print(Data_AddressBook.showPerson({}))(_12.value0);
                    };
                    throw new Error("Failed pattern match");
                })()();
                return Prelude.unit;
            };
        };
        throw new Error("Failed pattern match");
    });
    var setupEventHandlers = function __do() {
        Prelude[">>="](Control_Monad_Eff.bindEff({}))(Control_Monad_Eff_DOM.body)(Control_Monad_Eff_DOM.addEventListener("change")(validateAndUpdateUI))();
        return Prelude.unit;
    };
    return {
        setupEventHandlers: setupEventHandlers, 
        validateAndUpdateUI: validateAndUpdateUI, 
        validateControls: validateControls, 
        displayValidationErrors: displayValidationErrors, 
        valueOf: valueOf
    };
})();
var PS = PS || {};
PS.Data_JSON = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    function stringify(x) {  return JSON.stringify(x);};
    return {
        stringify: stringify
    };
})();
var PS = PS || {};
PS.Main = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Control_Monad_Eff = PS.Control_Monad_Eff;
    var Control_Monad_Eff_DOM = PS.Control_Monad_Eff_DOM;
    var Data_Maybe = PS.Data_Maybe;
    var Data_AddressBook = PS.Data_AddressBook;
    var Debug_Trace = PS.Debug_Trace;
    var Data_AddressBook_UI = PS.Data_AddressBook_UI;
    var Data_Either = PS.Data_Either;
    var Data_Array = PS.Data_Array;
    var Control_Monad_Eff_Storage = PS.Control_Monad_Eff_Storage;
    var Data_JSON = PS.Data_JSON;
    var Data_Foreign = PS.Data_Foreign;
    var FormData = {
        create: function (value) {
            return value;
        }
    };
    function alert(msg) {  return function() {    window.alert(msg);    return {};  };};
    var updateForm = function (sel) {
        return function (value) {
            return Prelude[">>="](Control_Monad_Eff.bindEff({}))(Control_Monad_Eff_DOM.querySelector(sel))(function (_21) {
                if (_21 instanceof Data_Maybe.Just) {
                    return function __do() {
                        Control_Monad_Eff_DOM.setValue(value)(_21.value0)();
                        return Prelude.unit;
                    };
                };
                throw new Error("Failed pattern match");
            });
        };
    };
    var toFormData = function (_282) {
        if (_282.phones.length === 2) {
            return {
                firstName: _282.firstName, 
                lastName: _282.lastName, 
                street: _282.address.street, 
                city: _282.address.city, 
                state: _282.address.state, 
                homePhone: (_282.phones[0]).number, 
                cellPhone: (_282.phones[1]).number
            };
        };
        throw new Error("Failed pattern match");
    };
    var validateAndSaveEntry = function __do() {
        Debug_Trace.trace("Running validators")();
        var _23 = Data_AddressBook_UI.validateControls();
        (function () {
            if (_23 instanceof Data_Either.Left) {
                return alert("There are " + Prelude.show(Prelude.showNumber({}))(Data_Array.length(_23.value0)) + " validation errors.");
            };
            if (_23 instanceof Data_Either.Right) {
                return function __do() {
                    Control_Monad_Eff_Storage.setItem("person")(Data_JSON.stringify(toFormData(_23.value0)))();
                    return alert("Saved")();
                };
            };
            throw new Error("Failed pattern match");
        })()();
        return Prelude.unit;
    };
    var readForeignFormData = function (__unused) {
        return new Data_Foreign.ReadForeign(Prelude[">>="](Data_Foreign.bindForeignParser({}))(Data_Foreign.prop(Data_Foreign.readString({}))("firstName"))(function (_20) {
            return Prelude[">>="](Data_Foreign.bindForeignParser({}))(Data_Foreign.prop(Data_Foreign.readString({}))("lastName"))(function (_19) {
                return Prelude[">>="](Data_Foreign.bindForeignParser({}))(Data_Foreign.prop(Data_Foreign.readString({}))("street"))(function (_18) {
                    return Prelude[">>="](Data_Foreign.bindForeignParser({}))(Data_Foreign.prop(Data_Foreign.readString({}))("city"))(function (_17) {
                        return Prelude[">>="](Data_Foreign.bindForeignParser({}))(Data_Foreign.prop(Data_Foreign.readString({}))("state"))(function (_16) {
                            return Prelude[">>="](Data_Foreign.bindForeignParser({}))(Data_Foreign.prop(Data_Foreign.readString({}))("homePhone"))(function (_15) {
                                return Prelude[">>="](Data_Foreign.bindForeignParser({}))(Data_Foreign.prop(Data_Foreign.readString({}))("cellPhone"))(function (_14) {
                                    return Prelude["return"](Data_Foreign.monadForeignParser({}))({
                                        firstName: _20, 
                                        lastName: _19, 
                                        street: _18, 
                                        city: _17, 
                                        state: _16, 
                                        homePhone: _15, 
                                        cellPhone: _14
                                    });
                                });
                            });
                        });
                    });
                });
            });
        }));
    };
    var loadSavedData = function __do() {
        var _22 = Control_Monad_Eff_Storage.getItem("person")();
        return (function () {
            var _400 = Data_Foreign.parseJSON(readForeignFormData({}))(_22);
            if (_400 instanceof Data_Either.Left) {
                return Prelude["return"](Control_Monad_Eff.monadEff({}))(Prelude.unit);
            };
            if (_400 instanceof Data_Either.Right) {
                return function __do() {
                    updateForm("#inputFirstName")(_400.value0.firstName)();
                    updateForm("#inputLastName")(_400.value0.lastName)();
                    updateForm("#inputStreet")(_400.value0.street)();
                    updateForm("#inputCity")(_400.value0.city)();
                    updateForm("#inputState")(_400.value0.state)();
                    updateForm("#inputHomePhone")(_400.value0.homePhone)();
                    updateForm("#inputCellPhone")(_400.value0.cellPhone)();
                    return Prelude.unit;
                };
            };
            throw new Error("Failed pattern match");
        })()();
    };
    var main = function __do() {
        Debug_Trace.trace("Loading data from local storage")();
        loadSavedData();
        Debug_Trace.trace("Attaching event handlers")();
        Data_AddressBook_UI.setupEventHandlers();
        return Prelude[">>="](Control_Monad_Eff.bindEff({}))(Control_Monad_Eff_DOM.querySelector("#saveButton"))(function (_24) {
            if (_24 instanceof Data_Maybe.Just) {
                return function __do() {
                    Control_Monad_Eff_DOM.addEventListener("click")(validateAndSaveEntry)(_24.value0)();
                    return Prelude.unit;
                };
            };
            throw new Error("Failed pattern match");
        })();
    };
    return {
        FormData: FormData, 
        main: main, 
        validateAndSaveEntry: validateAndSaveEntry, 
        loadSavedData: loadSavedData, 
        updateForm: updateForm, 
        toFormData: toFormData, 
        alert: alert, 
        readForeignFormData: readForeignFormData
    };
})();