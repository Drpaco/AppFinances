import re

with open('app.R', 'r', encoding='utf-8') as f:
    src = f.read()

# 1. Add library(bsicons) after library(ggplot2) if not already present
if 'library(bsicons)' not in src:
    src = src.replace('library(ggplot2)\n', 'library(ggplot2)\nlibrary(bsicons)\n', 1)

# 2. Insert dashboard source() call inside server, right after bump_db line
marker = 'bump_db <- function() db_tick(isolate(db_tick()) + 1)'
dashboard_block = (
    'bump_db <- function() db_tick(isolate(db_tick()) + 1)\n\n'
    '  # ------------------ Dashboard ------------------\n'
    '  source(file.path(app_dir, "R", "dashboard.R"), local = TRUE)\n'
)
if 'dashboard.R' not in src:
    src = src.replace(marker, dashboard_block.rstrip('\n'), 1)

with open('app.R', 'w', encoding='utf-8') as f:
    f.write(src)

print('Patched OK')
