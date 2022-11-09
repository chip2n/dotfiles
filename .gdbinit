set history save on
set history size 10000
set history filename ~/.gdb_history

# Disable quit confirm
define hook-quit
    set confirm off
end
