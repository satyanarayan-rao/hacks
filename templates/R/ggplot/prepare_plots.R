prep_r_square_plot <- function (data_frame, 
                                x_in_string_format, 
                                y_in_string_format, 
                                group_or_color_variable_in_string_format, 
                                r_squared, point_size, 
                                x_axis_title_in_string_format,
                                y_axis_title_in_string_format ){
    max_x = max (data_frame [, x_in_string_format] )
    max_y = max (data_frame [, y_in_string_format] )
    max_of_both = max (max_x, max_y) + 1
    min_x = min (data_frame [, x_in_string_format] )
    min_y = min (data_frame [, y_in_string_format] )
    min_of_both = min (min_x, min_y) - 1
    max_of_both = round (max_of_both, 2)
    min_of_both = round (min_of_both, 2)

    tics = max ( round (max_of_both/10), 1 )
    if (max_of_both%%tics != 0 ) {
        max_of_both = max_of_both + tics
    }
    g = ggplot (data_frame, aes_string ( x = x_in_string_format, y = y_in_string_format))
    scatter = geom_point ( aes_string (color = group_or_color_variable_in_string_format), size = point_size)
    #axis_min_max_and_tics = scale_x_continuous ("Original response" , breaks = seq (0,max_of_both, tics), limits = c (0, max_of_both)  ) + scale_y_continuous ("Predicted response" , breaks = seq (0,max_of_both, tics), limits = c (0, max_of_both)  )  
    #axis_min_max_and_tics_y = scale_y_continuous (parse (text = paste ("Delta ~ Delta ~  Delta~ hat(G) " )) , breaks = seq (0,max_of_both, tics), limits = c (0, max_of_both)  )
    axis_min_max_and_tics_y = scale_y_continuous (parse (text = y_axis_title_in_string_format ) , breaks = seq (min_of_both,max_of_both, tics), limits = c (min_of_both, max_of_both)  )
    axis_min_max_and_tics_x = scale_x_continuous (parse (text = x_axis_title_in_string_format), limits = c(min_of_both,max_of_both), breaks = seq(min_of_both, max_of_both, tics)  )
    expr =  paste ("R^{2}==", r_squared, sep = " " )
    annotation = annotate ("text", x = max_of_both/2  , y = max_of_both , label= expr, parse = TRUE, size = 10)
    line = geom_abline (slope = 1, intercept = 0)

    p = g + themes  + scatter + axis_min_max_and_tics_y + axis_min_max_and_tics_x + annotation + line + coord_fixed()

    return (p)
}

prep_density_plot <- function (data_frame, 
                               x_in_string_format, 
                               group_or_color_variable_in_string_format, 
                               x_axis_title_in_string_format, 
                               y_axis_title_in_string_format, 
                               alpha) {
    g = ggplot (data_frame, aes_string (x = x_in_string_format, y = "..scaled.."))
    density = geom_density (aes_string (group = group_or_color_variable_in_string_format,
                                        fill = group_or_color_variable_in_string_format, 
                                        color = group_or_color_variable_in_string_format) ,
                            alpha = alpha)
    x_axis_title = scale_x_continuous (parse (text = x_axis_title_in_string_format)  )
    y_axis_title = scale_y_continuous (parse (text = y_axis_title_in_string_format)  )

    p = g + density + themes + x_axis_title + y_axis_title

    return (p)

}

