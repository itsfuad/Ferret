export function createFerretRuntime() {
  let memory = null;
  let heapPtr = 0;
  const decoder = new TextDecoder("utf-8");

  function align(value, alignment) {
    const mask = alignment - 1;
    return (value + mask) & ~mask;
  }

  function bind(instance) {
    memory = instance.exports.memory;
    const dataEndExport = instance.exports.__data_end;
    const dataEnd =
      dataEndExport && typeof dataEndExport === "object"
        ? Number(dataEndExport.value)
        : Number(dataEndExport || 0);
    heapPtr = align(dataEnd, 8);
  }

  function view() {
    if (!memory) {
      throw new Error("Ferret runtime not bound to memory");
    }
    return new DataView(memory.buffer);
  }

  function readCString(ptr) {
    const bytes = [];
    const mem = new Uint8Array(memory.buffer);
    let i = ptr >>> 0;
    while (i < mem.length) {
      const b = mem[i++];
      if (b === 0) break;
      bytes.push(b);
    }
    return decoder.decode(new Uint8Array(bytes));
  }

  function ferret_alloc(size) {
    const bytes = Number(size);
    const addr = heapPtr;
    heapPtr = align(heapPtr + bytes, 8);
    return addr >>> 0;
  }

  function ferret_memcpy(dst, src, size) {
    const bytes = Number(size);
    const mem = new Uint8Array(memory.buffer);
    mem.copyWithin(dst >>> 0, src >>> 0, (src >>> 0) + bytes);
  }

  function ferret_array_new(elemSize, cap) {
    const elemBytes = Number(elemSize);
    const capacity = Number(cap);
    const dataSize = elemBytes * capacity;
    const dataPtr = dataSize > 0 ? ferret_alloc(dataSize) : 0;
    const arrPtr = ferret_alloc(16);
    const dv = view();
    dv.setUint32(arrPtr + 0, dataPtr, true);
    dv.setInt32(arrPtr + 4, 0, true);
    dv.setInt32(arrPtr + 8, capacity, true);
    dv.setUint32(arrPtr + 12, elemBytes, true);
    return arrPtr >>> 0;
  }

  function ferret_array_append(arrPtr, elemPtr) {
    const dv = view();
    const dataPtr = dv.getUint32(arrPtr + 0, true);
    const length = dv.getInt32(arrPtr + 4, true);
    const capacity = dv.getInt32(arrPtr + 8, true);
    const elemSize = dv.getUint32(arrPtr + 12, true);
    if (length >= capacity) {
      return 0;
    }
    const dest = dataPtr + length * elemSize;
    ferret_memcpy(dest, elemPtr, elemSize);
    dv.setInt32(arrPtr + 4, length + 1, true);
    return 1;
  }

  function ferret_array_get(arrPtr, index) {
    const dv = view();
    const dataPtr = dv.getUint32(arrPtr + 0, true);
    const length = dv.getInt32(arrPtr + 4, true);
    const elemSize = dv.getUint32(arrPtr + 12, true);
    if (index < 0 || index >= length) {
      return 0;
    }
    return (dataPtr + index * elemSize) >>> 0;
  }

  function ferret_array_set(arrPtr, index, elemPtr) {
    const dv = view();
    const dataPtr = dv.getUint32(arrPtr + 0, true);
    const length = dv.getInt32(arrPtr + 4, true);
    const elemSize = dv.getUint32(arrPtr + 12, true);
    if (index < 0 || index >= length) {
      return 0;
    }
    ferret_memcpy(dataPtr + index * elemSize, elemPtr, elemSize);
    return 1;
  }

  function ferret_array_len(arrPtr) {
    const dv = view();
    return dv.getInt32(arrPtr + 4, true);
  }

  function ferret_array_cap(arrPtr) {
    const dv = view();
    return dv.getInt32(arrPtr + 8, true);
  }

  function printUnion(ptr) {
    const dv = view();
    const tag = dv.getInt32(ptr, true);
    const data = ptr + 4;
    switch (tag) {
      case 0:
        return String(dv.getInt8(data));
      case 1:
        return String(dv.getInt16(data, true));
      case 2:
        return String(dv.getInt32(data, true));
      case 3:
        return String(dv.getBigInt64(data, true));
      case 4:
      case 5:
        return "<i128>";
      case 6:
        return String(dv.getUint8(data));
      case 7:
        return String(dv.getUint16(data, true));
      case 8:
        return String(dv.getUint32(data, true));
      case 9:
        return String(dv.getBigUint64(data, true));
      case 10:
      case 11:
        return "<u128>";
      case 12:
        return String(dv.getFloat32(data, true));
      case 13:
        return String(dv.getFloat64(data, true));
      case 14:
      case 15:
        return "<f128>";
      case 16: {
        const strPtr = dv.getUint32(data, true);
        return readCString(strPtr);
      }
      case 17: {
        const ch = dv.getUint8(data);
        return String.fromCharCode(ch);
      }
      case 18:
        return dv.getUint8(data) ? "true" : "false";
      default:
        return "<unknown>";
    }
  }

  function ferret_std_io_Print(slicePtr) {
    if (!slicePtr) return;
    const dv = view();
    const dataPtr = dv.getUint32(slicePtr + 0, true);
    const length = dv.getInt32(slicePtr + 4, true);
    const elemSize = dv.getUint32(slicePtr + 12, true);
    const parts = [];
    for (let i = 0; i < length; i++) {
      parts.push(printUnion(dataPtr + i * elemSize));
    }
    console.log(parts.join(" "));
  }

  function ferret_std_io_Println(slicePtr) {
    ferret_std_io_Print(slicePtr);
  }

  function ferret_panic(msgPtr) {
    const msg = msgPtr ? readCString(msgPtr) : "panic";
    throw new Error(msg);
  }

  function ferret_string_len(ptr) {
    const mem = new Uint8Array(memory.buffer);
    let i = ptr >>> 0;
    let len = 0;
    while (i < mem.length && mem[i] !== 0) {
      len++;
      i++;
    }
    return len;
  }

  return {
    bind,
    imports: {
      ferret: {
        ferret_alloc,
        ferret_memcpy,
        ferret_array_new,
        ferret_array_append,
        ferret_array_get,
        ferret_array_set,
        ferret_array_len,
        ferret_array_cap,
        ferret_std_io_Print,
        ferret_std_io_Println,
        ferret_panic,
        ferret_string_len,
      },
    },
  };
}
