######################################################################
## Copyright (C) 2014, Dave Straube, http://davestraube.com
##     
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
## 
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
## 
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA
######################################################################

# This program implements a performance test suite for various flavors
# of filehash key-value databases.  Make sure NOT to include filehashSQLite.

if ( "package:filehashSQLite" %in% search() ) {
    print("filehashSQLite must be unloaded before running this program.  Run:")
    print("    detach(name=\"package:filehashSQLite\", unload=TRUE, character.only=TRUE, force=TRUE)")
    print("and restart your R session before running this program.")
    quit()
}

print(getwd())
rm(list=ls())

library(stringi)
library(RSQLite)
library(filehash)

# Include modified filehasSQLite class and register it.
source("./filehash-SQLite.R", echo=TRUE)
registerFormatDB("SQLite", list(create = createSQLite, initialize = initializeSQLite))

# Pretty print helper.
fmt <- function(x) { format(x, big.mark=",", scientific=FALSE) }

# All keys will be of the form "key_N" where N is a zero-prefixed, 10-digit number.
MakeKey <- function(n) {
    sprintf("key_%010d", n)
}




# Performance test routine for dbInsert.  Returns operations/sec. value.
Write <- function(count) {
    print(stri_join("    Write(", fmt(count), ") ..."))
    write <- function(n) { 
        dbInsert(db, MakeKey(n), n)
    }
    start <- Sys.time()
    junk <- sapply(seq(1, count), write)
    end <- Sys.time()
    count / as.numeric(difftime(end, start, units="secs"))
}

# Performance test routine for dbFetch.  Returns operations/sec. value.
Read <- function(count) {
    print(stri_join("    Read(", fmt(count), ") ..."))
    read <- function(n) {
        dbFetch(db, MakeKey(n))
    }
    # Read count values, but select them randomly.
    set.seed(1234)
    readvals <- sample(seq(1, count), count, replace=TRUE)
    start <- Sys.time()
    junk <- sapply(readvals, read)
    end <- Sys.time()
    stopifnot(junk == readvals)
    count / as.numeric(difftime(end, start, units="secs"))
}

# Performance test routine for dbList.  Returns operations/sec. value.
List <- function(count) {
    print(stri_join("    List(", fmt(count), ") ..."))
    start <- Sys.time()
    x <- dbList(db)
    end <- Sys.time()
    stopifnot(count == length(x) & count == sum(sort(x) == sapply(seq(1, count), MakeKey)))
    count / as.numeric(difftime(end, start, units="secs"))
}

# Performance test routine for dbExists.  Returns operations/sec. value.
Exists <- function(count) {
    print(stri_join("    Exists(", fmt(count), ") ..."))
    exists <- function(n) {
        dbExists(db, MakeKey(n))
    }
    # Read only 10% of count under assumption most apps are not dbExists intensive.
    # Read values chosen at random.
    set.seed(1234)
    excount <- floor(.1 * count)
    readvals <- sample(seq(1, count), excount, replace=TRUE)
    start <- Sys.time()
    junk <- sapply(readvals, exists)
    end <- Sys.time()
    # RDS data stores often fail to find an existing key on first pass.
    # Abort if retry fails as well.  Don't understand this failure mode yet ...
    if ( excount != sum(junk) ) {
        df <- data.frame(readvals=readvals, junk=junk)
        print(stri_join("***** ", sum(!df$junk), " bad dbExists() - verifying ..."))
        for ( idx in which(!df$junk) ) {
            stopifnot(dbExists(db, MakeKey(df$readvals[idx])))
        }
    }
    excount / as.numeric(difftime(end, start, units="secs"))
}

# Consistency checking routine to verify original DB1, RDS and SQLite variant behaviors
# and that we aren't breaking any semantics with the SQLite+ modifications.
isConsistent <- function() {
    # Returns "OK" or error string.
    suppressWarnings({
        keys <- c("key1", "key2", "key3")
        vals <- c(1, 2, 3)
        for ( i in seq(length(keys)) ) { dbInsert(db, keys[i], vals[i]) }
        if ( ! identical(keys, dbList(db)) ) { return("dbList") }
        for ( i in seq(length(keys)) ) { if ( vals[i] != dbFetch(db, keys[i]) ) { return("dbFetch") } }
        if ( ! identical(keys, names(dbMultiFetch(db, keys))) ) { return("dbMultiFetch") }
        if ( ! identical(vals, unlist(dbMultiFetch(db, keys), use.names=FALSE)) ) { return("dbMultiFetch") }
        dbInsert(db, "key1", 22)
        dbInsert(db, "key1", 99)
        x <- dbMultiFetch(db, "key1")
        if ( 1 != length(x) || 99 != x[[1]] ) { return("replace") }
        for ( i in seq(1, length(keys)) ) { dbDelete(db, keys[i]) }
        if ( 0 != length(dbList(db)) ) { return("dbDelete") }
    })
    "OK"
}

# Data frame documenting consistency checking parameters.
# See head of filehash-SQLite.R for meaning of KEY_INDEX, etc.
tests <- data.frame(
    dbname = c("db_DB1", "db_RDS", "db_SQLite", "db_SQLite_Plus"),
    dbtype = c("DB1", "RDS", "SQLite", "SQLite"),
    KEY_INDEX = c(NA, NA, FALSE, TRUE),
    DELETE_BEFORE_ADD = c(NA, NA, TRUE, FALSE),
    INSERT_OR_REPLACE = c(NA, NA, FALSE, TRUE),
    EXISTS_BY_QUERY = c(NA, NA, FALSE, TRUE),
    consistent = c(NA, NA, NA, NA),
    stringsAsFactors=FALSE)

# Execute all consistency checking test cases and print results.
for ( testidx in seq(dim(tests)[1]) ) {
    KEY_INDEX <- tests[testidx, "KEY_INDEX"]
    DELETE_BEFORE_ADD <- tests[testidx, "DELETE_BEFORE_ADD"]
    INSERT_OR_REPLACE <- tests[testidx, "INSERT_OR_REPLACE"]
    EXISTS_BY_QUERY <- tests[testidx, "EXISTS_BY_QUERY"]
    dbname <- tests[testidx, "dbname"]
    if ( file.exists(dbname) ) { file.remove(dbname) }
    dbCreate(dbname, type=tests[testidx, "dbtype"])
    db <- dbInit(dbname, type=tests[testidx, "dbtype"])
    tests[testidx, "consistent"] <- isConsistent()
    dbUnlink(db)
    db <- NULL
}
print(tests)
stopifnot(dim(tests)[1] == sum(tests$consistent == rep("OK", dim(tests)[1])))

# Define performance testing parameters.
testcounts <- c(10000, 25000, 50000, 100000, 250000)
ops <- c("Write", "Read", "List", "Exists")
empty_result <- data.frame(
    db_DB1 = rep(0, length(testcounts)),
    db_RDS = rep(0, length(testcounts)),
    db_SQLite = rep(0, length(testcounts)),
    db_SQLite_Plus = rep(0, length(testcounts)))
results <- list(
    Write = empty_result,
    Read = empty_result,
    List = empty_result,
    Exists = empty_result
)

# Routine for plotting performance test results.  Call this after each new data point is
# derived so plot updates itself dynamically as tests execute - assuming RStudio environment.
PlotResults <- function() {
    colors <- c("forestgreen", "red", "blue", "black")
    marks <- c(15, 16, 17, 18)
    par(mfrow=c(2,2))
    for ( op in ops ) {
        plot(c(testcounts[1], testcounts[length(testcounts)]),
             c(0, 1.1 * max(as.matrix(results[[op]]))),
             type="n",
             main=op,
             xlab="Item count",
             ylab="Operations per second")
        legend(x="topright", legend=tests$dbname, col=colors, pch=marks,
               x.intersp=1, y.intersp=1, cex=1.0, bty="n")
        for ( testidx in seq(dim(tests)[1]) ) {
            points(testcounts, results[[op]][, tests$dbname[testidx]], col=colors[testidx], pch=marks[testidx])
            lines(testcounts, results[[op]][, tests$dbname[testidx]], col=colors[testidx])
        }
    }
}

# Execute performance tests.
# Warning - best to leave your machine unattended with sleep mode disabled overnight!
for ( countidx in seq(length(testcounts)) ) {
    count <- testcounts[countidx]
    for ( testidx in seq(dim(tests)[1]) ) {
        KEY_INDEX <- tests[testidx, "KEY_INDEX"]
        DELETE_BEFORE_ADD <- tests[testidx, "DELETE_BEFORE_ADD"]
        INSERT_OR_REPLACE <- tests[testidx, "INSERT_OR_REPLACE"]
        EXISTS_BY_QUERY <- tests[testidx, "EXISTS_BY_QUERY"]
        dbname <- tests[testidx, "dbname"]
        if ( file.exists(dbname) ) { file.remove(dbname) }
        dbtype <- tests[testidx, "dbtype"]
        dbCreate(dbname, type=dbtype)
        db <- dbInit(dbname, type=dbtype)
        print(tests[testidx, "dbname"])
        for ( op in ops ) {
            # Speed things up if prior test in category resulted in less than 50 ops/sec.
            # Whack performance value to 0 ops/sec since <50 is essentially useless anyway.
            if ( countidx > 1 && results[[op]][countidx-1, testidx] < 50 ) {
                delta <- 0
            } else {
                delta <- NULL
                if ( "Write" == op ) { delta <- Write(count) }
                if ( "Read" == op ) { delta <- Read(count) }
                if ( "List" == op ) { delta <- List(count) }
                if ( "Exists" == op ) { delta <- Exists(count) }
                stopifnot(!is.null(delta))
            }
            results[[op]][countidx, testidx] <- delta
            PlotResults()
        }
        dbUnlink(db)
        db <- NULL
    }
}


