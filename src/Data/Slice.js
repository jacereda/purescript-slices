"use strict";

// module Data.Slice

exports.sfind = function(f) {
  return function(a) {
    for (var i = a.base>>>0, l = (a.base + a.len)>>>0; i < l; i++)
      if (f(a.arr[i]))
        return i - a.base;
    return -1;
  };
}

exports.sfindLast = function(f) {
  return function(a) {
    for (var i = a.base + a.len - 1; i >= a.base; i--)
      if (f(a.arr[i]))
        return i - a.base;
    return -1;
  };
}

exports.smap = function(f) {
  return function (a) {
    var l = a.len >>> 0;
    var r = {base:0, len:l, arr:new Array(l)};
    for (var i = 0 >>> 0; i < l; i++)
      r.arr[i] = f(a.arr[a.base+i]);
    return r;
  };
}

exports.sfoldl = function(f) {
  return function(z) {
    return function (a) {
      var r = z;
      var l = a.len >>> 0;
      var a1 = {base:0, len:l, arr:new Array(l)};
      for (var i = 0 >>> 0; i < l; i++)
        r = f(r)(a.arr[a.base+i]);
      return r;
    };
  };
}

exports.sfoldr = function(f) {
  return function(z) {
    return function (a) {
      var r = z;
      var l = a.len >>> 0;
      var a1 = {base:0, len:l, arr:new Array(l)};
      while (l--)
        r = f(a.arr[a.base+l])(r);
      return r;
    };
  };
}

exports.szipWith = function(f) {
  return function(a) {
    return function(b) {
      var l = Math.min(a.len, b.len) >>> 0;
      var r = {base:0, len:l, arr:new Array(l)};
      for (var i = 0; i < l; i++)
        r.arr[i] = f(a.arr[a.base+i])(b.arr[b.base+i]);
      return r;
    };
  };
}
