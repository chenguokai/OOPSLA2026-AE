import lief
import sys

binary = lief.parse(sys.argv[1])

for i in range(len(binary.sections) - 1):
    current_section = binary.sections[i]
    next_section = binary.sections[i + 1]
    
    gap_start = current_section.offset + current_section.size
    gap_size = next_section.offset - gap_start
    
    if gap_size > 0 and gap_size < 1000:
        # print gap_start in hex
        print(f"Gap at 0x{gap_start:08x} with size {gap_size} bytes")
        # Find the segment that contains this gap
        for segment in binary.segments:
            if (segment.file_offset <= gap_start and 
                segment.file_offset + segment.physical_size >= gap_start + gap_size):
                
                # Get the segment content
                content = list(segment.content)
                
                # Calculate the relative offset within the segment
                relative_start = gap_start - segment.file_offset
                
                # Extend content size if needed
                required_size = relative_start + gap_size
                if required_size > len(content):
                    # Extend content with zeros up to required size
                    content.extend([0] * (required_size - len(content)))
                
                # Fill the gap with NOPs
                for j in range(gap_size >> 1):
                    content[relative_start + j * 2] = 0x01
                    content[relative_start + j * 2 + 1] = 0x00
                
                # Update segment content
                segment.content = content
                
                # Update segment size attributes if needed
                if required_size > segment.physical_size:
                    segment.physical_size = required_size
                    # In most cases, virtual size should be aligned
                    alignment = 0x1000  # typical page size
                    segment.virtual_size = ((required_size + alignment - 1) // alignment) * alignment
                break

binary.write(sys.argv[2])