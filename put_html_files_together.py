# for directories of session_01_* to session_12_* read the .html file inside
# and put them in a single directory called html_files_together, then rename them
# by prepending the "<session-number>_" to the filename

import glob
import os
import shutil

# create the target directory if it doesn't exist
target_dir = "html_files_together"
os.makedirs(target_dir, exist_ok=True)

# iterate over the session directories
for i in range(1, 13):  # from session_01 to session_12
    session_dir = f"session_{i:02d}_*"
    # inside each session directory, find the one .html file
    html_file = glob.glob(os.path.join(session_dir, "*.html"))[0]
    # construct the new filename
    new_filename = f"{i:02d}_" + os.path.basename(html_file)
    # copy the file to the target directory with the new name
    shutil.copy(html_file, os.path.join(target_dir, new_filename))
    print(f"Copied {html_file} to {os.path.join(target_dir, new_filename)}")
