#!/usr/bin/env python3
import csv
import sys
from datetime import datetime

def convert_date_format(input_file, output_file):
    try:
        with open(input_file, 'r') as original_file, \
             open(output_file, 'w', newline='') as modified_file:
            reader = csv.reader(original_file)
            writer = csv.writer(modified_file)

            # Write the header row to the new CSV file
            header = next(reader)
            writer.writerow(header)

            # Convert and write the data rows with date format conversion
            for row in reader:
                date_str = row[7]  # Assuming the date/time is in the 8th column
                date_obj = datetime.strptime(date_str, '%d/%m/%Y %H:%M:%S')  # Adjust the format string
                row[7] = date_obj.strftime('%Y-%m-%d %H:%M:%S')
                writer.writerow(row)

    except Exception as e:
        print(f"An error occurred: {e}")

if __name__ == '__main__':
    if len(sys.argv) != 3:
        print("Usage: python date_converter.py input.csv output.csv")
        sys.exit(1)

    input_file = sys.argv[1]
    output_file = sys.argv[2]
    convert_date_format(input_file, output_file)

