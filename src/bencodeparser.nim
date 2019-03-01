# bencodeparser
# Copyright xmonader
# bencode library

import strformat, tables, strutils, hashes

# or if you want a distinct type
# type btString = distinct string
type 
    BencodeKind* = enum
        btString, btInt, btList, btDict

    BencodeType* = ref object
        case kind*: BencodeKind 
        of BencodeKind.btString: s* : string 
        of BencodeKind.btInt: i*    : int
        of BencodeKind.btList: l*   : seq[BencodeType]
        of BencodeKind.btDict: d*  : OrderedTable[BencodeType, BencodeType]
    
    Encoder* = ref object
    Decoder* = ref object 

proc hash*(obj: BencodeType): Hash = 
    case obj.kind
    of btString : !$(hash(obj.s))
    of btInt : !$(hash(obj.i))
    of btList: !$(hash(obj.l))
    of btDict: 
        var h = 0
        for k, v in obj.d.pairs:
            h = hash(k) !& hash(v)
        !$(h)

proc `$`* (a: BencodeType): string = 
    case a.kind
    of btString:  fmt("<Bencode {a.s}>")
    of btInt: fmt("<Bencode {a.i}>")
    of btList: fmt("<Bencode {a.l}>")
    of btDict: fmt("<Bencode {a.d}")
        
proc `==`* (a, b: BencodeType): bool =
    ## Check two nodes for equality
    if a.isNil:
        if b.isNil: return true
        return false
    elif b.isNil or a.kind != b.kind:
        return false
    else:
        case a.kind
        of btString:
            result = a.s == b.s
        of btInt:
            result = a.i == b.i
        of btList:
            result = a.l == b.l
        of btDict:
            if a.d.len != b.d.len: return false
            for key, val in a.d:
                if not b.d.hasKey(key): return false
                if b.d[key] != val: return false
            result = true



proc encode(this: Encoder,  obj: BencodeType) : string

proc encode_s(this: Encoder, s: string) : string=
    # TODO: check len
    return $s.len & ":" & s

proc encode_i(this: Encoder, i: int) : string=
    # TODO: check len
    return fmt("i{i}e") 
    
proc encode_l(this: Encoder, l: seq[BencodeType]): string =
    var encoded = "l"
    for el in l:
        encoded &= this.encode(el)
    encoded &= "e"
    return encoded

proc encode_d(this: Encoder, d: OrderedTable[BencodeType, BencodeType]): string =
    var encoded = "d"
    for k, v in d.pairs():
        assert k.kind == BencodeKind.btString
        encoded &= this.encode(k) & this.encode(v)

    encoded &= "e"
    return encoded

proc encode(this: Encoder,  obj: BencodeType) :  string =
    case obj.kind
    of BencodeKind.btString:  result =this.encode_s(obj.s)
    of BencodeKind.btInt :  result = this.encode_i(obj.i)
    of BencodeKind.btList : result = this.encode_l(obj.l)
    of BencodeKind.btDict : result = this.encode_d(obj.d)

proc decode(this: Decoder,  source: string) : (BencodeType, int)

proc decode_s(this: Decoder, s: string) : (BencodeType, int) =
    let lengthpart = s.split(":")[0]
    let sizelength = lengthpart.len
    let strlen = parseInt(lengthpart)
    return (BencodeType(kind:btString, s: s[sizelength+1..sizelength+strlen]), sizelength+1+strlen)

proc decode_i(this: Decoder, s: string) : (BencodeType, int) =
    let epos = s.find('e')
    let i = parseInt(s[1..<epos])
    return (BencodeType(kind:btInt, i:i), epos+1)
    
proc decode_l(this: Decoder, s: string): (BencodeType, int) =
    # l ... e
    var els = newSeq[BencodeType]()
    var curchar = s[1]
    var idx = 1
    while idx < s.len:
        curchar = s[idx]
        if curchar == 'e':
            idx += 1
            break
    
        let pair = this.decode(s[idx..<s.len])
        let obj = pair[0]
        let nextobjpos = pair[1] 
        els.add(obj)
        idx += nextobjpos
    return (BencodeType(kind:btList, l:els), idx)

proc decode_d(this: Decoder, s: string): (BencodeType, int) =
    var d = initOrderedTable[BencodeType, BencodeType]()
    var curchar = s[1]
    var idx = 1
    var readingKey = true
    var curKey: BencodeType
    while idx < s.len:
        curchar = s[idx]
        if curchar == 'e':
            break
        let pair = this.decode(s[idx..<s.len])
        let obj = pair[0]
        let nextobjpos = pair[1]
        if readingKey == true:
            curKey = obj
            readingKey = false
        else:
            d[curKey] = obj
            readingKey = true
        idx += nextobjpos
    return (BencodeType(kind:btDict, d: d), idx)

proc decode(this: Decoder,  source: string) : (BencodeType, int) =
    var curchar = source[0]
    var idx = 0
    while idx < source.len:
        curchar = source[idx]
        case curchar
        of 'i':
            let pair = this.decode_i(source[idx..^1])
            let obj = pair[0]
            let nextobjpos = pair[1] 
            idx += nextobjpos
            return (obj, idx)
        of 'l':
            let pair = this.decode_l(source[idx..^1])
            let obj = pair[0]
            let nextobjpos = pair[1] 
            idx += nextobjpos
            return (obj, idx)
        of 'd':
            let pair = this.decode_d(source[idx..^1])
            let obj = pair[0]
            let nextobjpos = pair[1] 
            idx += nextobjpos
            return (obj, idx)
        else: 
            let pair = this.decode_s(source[idx..^1])
            let obj = pair[0]
            let nextobjpos = pair[1] 
            idx += nextobjpos
            return (obj, idx)


proc newEncoder*(): Encoder =
    new Encoder

proc newDecoder*(): Decoder = 
    new Decoder

proc encodeObject*(this: Encoder, obj: BencodeType) : string =
    return this.encode(obj)

proc decodeObject*(this: Decoder, source:string) : BencodeType =
    let p = this.decode(source)
    return p[0]

