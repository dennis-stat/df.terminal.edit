#' Get value from the terminal prompt
#' @param variable_name = name of the variable
#' @param current_value = the current value
#' @param example_value = for example (optional)

get_value_from_prompt <- function(
    variable_name = 'variable',
    current_value = '',
    example_value = 'Value123',
    show_result = F
) {
    # Prompt for value
    cat(  paste('\n Please enter value for \'',
                variable_name, '\'',
                ifelse(nchar(example_value)>0, paste(' e.g. \'', example_value, '\'',sep = ''),''),
                ' (current value: \'',current_value, '\', to keep it just press ENTER)    :', sep = ''))

    new_value <- readline()
    if(new_value == '') {new_value = current_value}
    if(show_result == T) {cat(paste('New value: \'', new_value,'\'\n', sep = ''))}
    return(new_value)
}

#' Edit one record from a data frame in terminal
#' @param record_to_edit  = record from a data frame to edit

edit_one_record_in_terminal <- function(
    # Default empty data frame
    record_to_edit = data.frame(parameter = c(''), value = c(''), stringsAsFactors = F)

) {
    # Extract variable names
    variable_names = colnames(record_to_edit)

    # Extract the current values
    current_values = (unlist(record_to_edit[1,], use.names = F))

    # Now ask for new values
    new_record <- mapply(get_value_from_prompt, variable_name = variable_names, current_value = current_values)

    return(new_record)
}

#' Edit data frame in terminal
#' @param df_to_edit  = data frame to edit
#' @param action with the following options:
#' = 1 (add new line)
#' = 2 (edit existing record)
#' = 3 (remove existing record)
#' = 4 (save and exit)
#'
#' @export
#' @examples
#' edit_data_frame_in_terminal(data_frame_to_edit)

edit_data_frame_in_terminal <- function(
    # Default empty data frame
    df_to_edit = data.frame(matrix(vector(), 0, 2,dimnames=list(c(), c("parameter", "value"))),stringsAsFactors=F),
    action = 0

) {
    # create row_number identifier and add it to the data frame
    number_of_records <- dim(df_to_edit)[1]
    column_names <- colnames(df_to_edit)
    row_numbers = data.frame(row_number = (if(number_of_records==0) c(1)[0] else (1:number_of_records)))
    # df_to_edit_rn = cbind(row_numbers, df_to_edit) # use it if we need additional column with numbers of rows

    cat('', sep="\n\n\n")
    cat('The currect values of the data frame:\n')
    print(df_to_edit)

    # Print edit options
    cat('', sep="\n\n\n")

    cat('To add new record please enter \'1\'\n')
    if(number_of_records>0) {cat('To edit a record please enter \'2\'\n')}
    if(number_of_records>0) {cat('To remove a record please enter \'3\'\n')}
    cat('To save and exit please just press \'Enter\' or type anything else\n')

    action <- readline()



    if (action == 1) {
        # If choice is to add add new record
        new_record <- edit_one_record_in_terminal(df_to_edit[number_of_records+1,])
        df_new <- rbind(df_to_edit, new_record,stringsAsFactors=F)
        colnames(df_new) <- column_names
        df_new <- edit_data_frame_in_terminal(df_to_edit = df_new)
        return(df_new)
    }

    if (action == 2) {
        # If choice is to edit existing record
      cat('please enter row_number of the record you want to edit\n')
        row_number_to_edit <- readline()
        if(row_number_to_edit %in% 1:number_of_records) {
        new_record <- edit_one_record_in_terminal(df_to_edit[row_number_to_edit,])
        df_to_edit[row_number_to_edit,] <- new_record
        colnames(df_to_edit) <- column_names
        df_new <- edit_data_frame_in_terminal(df_to_edit)
        return(df_new) } else {
          cat('Row number is out of scope\n')
            return(df_to_edit)
        }
        return(df_new)

    }


    if (action == 3) {
      # If choice is to edit existing record
      cat('please enter row_number of the record you want to remove\n')
      row_number_to_remove <- readline()
      if(row_number_to_remove %in% 1:number_of_records) {
        df_to_edit <- df_to_edit[1:number_of_records != row_number_to_remove,]
        colnames(df_to_edit) <- column_names
        df_new <- edit_data_frame_in_terminal(df_to_edit)
        return(df_new) } else {
          cat('Row number is out of scope\n')
          df_new <- edit_data_frame_in_terminal(df_to_edit)
        }
      return(df_new)

    }

    return(df_to_edit)
}



