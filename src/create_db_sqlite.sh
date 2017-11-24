#!/bin/bash
# Create SQLite Database for evidence assessment tool
sqlite3 db/loe.db < src/eat_sqlite.sql
