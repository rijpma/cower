# id subject
triplify = function(df, subjects){
    data.table::setnames(df, names(df), uriref(names(df), base = 'http://www.example.org/dimension/'))
    data.table::melt(df, id.vars = 1, 
        variable.name = 'pred', value.name = 'obj')
}