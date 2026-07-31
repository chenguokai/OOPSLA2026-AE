#!/usr/bin/env python3
from elftools.elf.elffile import ELFFile
from elftools.dwarf.descriptions import describe_form_class
import sys
import json

def extract_symbols_and_lines(filename):
    with open(filename, 'rb') as f:
        elffile = ELFFile(f)

        if not elffile.has_dwarf_info():
            print("This ELF file has no DWARF debug info.")
            sys.exit(1)

        dwarfinfo = elffile.get_dwarf_info()

        variables = []
        functions = []
        addr_to_fileline = {}

        # ---- Parse functions & variables from DIE tree ----
        for CU in dwarfinfo.iter_CUs():
            top_DIE = CU.get_top_DIE()
            for DIE in top_DIE.iter_children():
                # ---- Functions ----
                if DIE.tag == 'DW_TAG_subprogram':
                    name_attr = DIE.attributes.get('DW_AT_name')
                    low_pc_attr = DIE.attributes.get('DW_AT_low_pc')
                    if low_pc_attr:
                        addr = low_pc_attr.value
                        name = name_attr.value.decode('utf-8', errors='replace') if name_attr else "<unnamed>"
                        functions.append((name, addr))

                # ---- Variables ----
                elif DIE.tag == 'DW_TAG_variable':
                    name_attr = DIE.attributes.get('DW_AT_name')
                    loc_attr = DIE.attributes.get('DW_AT_location')
                    if loc_attr and describe_form_class(loc_attr.form) == 'exprloc':
                        expr = loc_attr.value
                        if expr and expr[0] == 0x03:  # DW_OP_addr opcode
                            addr = int.from_bytes(expr[1:], byteorder='little')
                            name = name_attr.value.decode('utf-8', errors='replace') if name_attr else "<unnamed>"
                            variables.append((name, addr))

        # ---- Parse line program (file:line mapping) ----
        for CU in dwarfinfo.iter_CUs():
            lineprog = dwarfinfo.line_program_for_CU(CU)
            if not lineprog:
                continue
            for entry in lineprog.get_entries():
                if entry.state is None or entry.state.end_sequence:
                    continue
                addr = entry.state.address
                file_entry = lineprog['file_entry'][entry.state.file]
                filename = file_entry.name.decode('utf-8', errors='replace')
                dir_index = file_entry.dir_index
                if dir_index != 0:
                    dir_name = lineprog['include_directory'][dir_index].decode('utf-8', errors='replace')
                    full_path = f"{dir_name}/{filename}"
                else:
                    full_path = filename
                line = entry.state.line
                addr_to_fileline[addr] = (full_path, line)

        return variables, functions, addr_to_fileline

def build_fileline_to_addrs(addr_to_fileline):
    """Group and sort addresses by (file, line), keep the lowest address as the representative"""
    fileline_to_addrs = {}

    # Group addresses by (file, line)
    for addr, (file, line) in addr_to_fileline.items():
        key = (file, line)
        if key not in fileline_to_addrs:
            fileline_to_addrs[key] = []
        fileline_to_addrs[key].append(addr)

    # Sort addresses for each (file, line) pair
    for key in fileline_to_addrs:
        fileline_to_addrs[key].sort()

    return fileline_to_addrs

def interactive_lookup(addr_to_fileline, fileline_to_addrs):
    print("\n🔍 Interactive lookup (type 'exit' to quit)")
    print(" - Enter address (e.g., 0x400530)")
    print(" - Or enter source location (e.g., path/to/file.c:42)")
    #print(fileline_to_addrs)
    while True:
        query = input("\nLookup> ").strip()
        if query.lower() in ('exit', 'quit'):
            break

        # ---- Try address lookup ----
        if query.startswith("0x"):
            try:
                addr = int(query, 16)
            except ValueError:
                print("❌ Invalid address format. Use hex like 0x400530.")
                continue

            if addr in addr_to_fileline:
                f, line = addr_to_fileline[addr]
                print(f"📍 Address 0x{addr:x} → {f}:{line}")
            else:
                lower_addrs = [a for a in addr_to_fileline.keys() if a <= addr]
                if lower_addrs:
                    nearest = max(lower_addrs)
                    f, line = addr_to_fileline[nearest]
                    print(f"📍 Closest match: 0x{nearest:x} → {f}:{line}")
                else:
                    print("⚠️ No source info found for that address.")

        # ---- Try file:line lookup ----
        elif ":" in query:
            try:
                file_part, line_part = query.rsplit(":", 1)
                line_num = int(line_part)
            except ValueError:
                print("❌ Invalid format. Use something like foo.c:42")
                continue

            # Allow partial filename match (suffix match)
            matches = [
                (f, l, addrs[0])
                for (f, l), addrs in fileline_to_addrs.items()
                if f.endswith(file_part) and l == line_num
            ]

            if not matches:
                print("⚠️ No address found for that file:line.")
            else:
                for f, l, start_addr in matches:
                    print(f"📍 {f}:{l} → start address 0x{start_addr:x}")

        else:
            print("⚠️ Unrecognized input. Use 0xADDR or file:line format.")

def main():
    if len(sys.argv) != 2:
        print(f"Usage: {sys.argv[0]} <elf-file>")
        sys.exit(1)

    elf_file = sys.argv[1]
    variables, functions, addr_to_fileline = extract_symbols_and_lines(elf_file)
    fileline_to_addrs = build_fileline_to_addrs(addr_to_fileline)

    json_data = json.dumps([{"name": name, "addr": addr} for name, addr in variables], indent=2)
    json_data1 = json.dumps([{"name": name, "addr": addr} for name, addr in functions], indent=2)
    json_data2 = json.dumps(
        {f"{file}:{line}": addrs for (file, line), addrs in fileline_to_addrs.items()},
        indent=2
    )
    print(json_data)
    print("\n\n")
    print(json_data1)
    print("\n\n")
    print(json_data2)
if __name__ == "__main__":
    main()

