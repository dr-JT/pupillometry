#' A call to open the GUI window
#'
#' This function opens a GUI window for using pupil_preprocess()
#' @export
#'

pupil_gui <- function(){
  ## Create overall layout
  win <- gWidgets2::gwindow("Preprocess Raw Pupil Data", visible = FALSE)
  grp_win <- gWidgets2::ggroup(horizontal = TRUE, container = win)
  grp_all <- gWidgets2::ggroup(horizontal = FALSE, container = grp_win)

  ## First Row - Import, Output, and Miscelaneous
  grp_1 <- gWidgets2::ggroup(horizontal = TRUE, container = grp_all)

  ## Import ####
  frame_import <- gWidgets2::gframe(" Import Parameters ", horizontal = FALSE,
                         container = grp_1, pos = .5)
  lbl_import.dir <- gWidgets2::glabel("select import directory: ",
                           container = frame_import)
  obj_import.dir <- gWidgets2::gfilebrowse(text = "", type = "selectdir",
                                container = frame_import, quote=FALSE)
  form_import <- gWidgets2::gformlayout(container = frame_import, expand = TRUE)
  obj_pattern <- gWidgets2::gedit(".txt",
                       label = "import file pattern string",
                       container = form_import)
  obj_taskname <- gWidgets2::gedit("",
                        label = "task name",
                        container = form_import)
  obj_subj.prefix <- gWidgets2::gedit("",
                           label = "subject prefix",
                           container = form_import)
  obj_subj.suffix <- gWidgets2::gedit("",
                           label = "subject suffix",
                           container = form_import)

  lbl_timing.file <- gWidgets2::glabel("select timing file (optional)",
                            container = frame_import)
  obj_timing.file <- gWidgets2::gfilebrowse(text = "", container = frame_import,
                                 quote = FALSE)
  ########

  ## Create Output and Misc. Section
  grp_1.2 <- gWidgets2::ggroup(horizontal = FALSE, container = grp_1)

  ## Output ####
  frame_output <- gWidgets2::gframe(" Output Options ", horizontal = FALSE,
                         container = grp_1.2, pos = .5)
  lbl_output.dir <- gWidgets2::glabel("select output directory: ",
                                      container = frame_output)
  obj_output.dir <- gWidgets2::gfilebrowse(text = "", type = "selectdir",
                                container = frame_output, quote=FALSE)

  form_output <- gWidgets2::gformlayout(container = frame_output, expand = TRUE)
  obj_output.steps <- gWidgets2::gcombobox(c("TRUE", "FALSE"), select = 2,
                                label = "output files at each step?",
                                container = form_output)
  obj_files.merge <- gWidgets2::gcombobox(c("TRUE", "FALSE"), select = 2,
                               label = "create single merged file?",
                               container = form_output)
  ######

  ## Misc. ####
  frame_misc <- gWidgets2::gframe(" Miscelaneous Options ",
                                  horizontal = FALSE,
                                  container = grp_1.2, pos = .5)
  form_misc <- gWidgets2::gformlayout(container = frame_misc, expand = TRUE)
  obj_include_col <- gWidgets2::gedit("",
                      label = "include additional columns",
                      container = form_misc)
  obj_trial.exclude <- gWidgets2::gedit("",
                             label = "exclude trials",
                             container = form_misc)
  ########

  ## Second Row - Eye tracker and Message markers
  grp_2 <- gWidgets2::ggroup(horizontal = TRUE, container = grp_all)

  ## Eye Tracker ####
  frame_eyetracker <- gWidgets2::gframe(" Eye Tracker Information ",
                                        horizontal = FALSE,
                                        container = grp_2, pos = .5)
  form_eyetracker <- gWidgets2::gformlayout(container = frame_eyetracker,
                                            expand = TRUE)
  obj_eyetracker <- gWidgets2::gcombobox(c("tobii", "eyelink", "smi"),
                                         select = 2, label = "eye tracker",
                                         container = form_eyetracker)
  obj_hz <- gWidgets2::gedit("",
                  label = "recording frequency (Hz)",
                  container = form_eyetracker)
  obj_eye.use <- gWidgets2::gcombobox(c("left", "right"), select = 1,
                           label = "use left or right eye?",
                           container = form_eyetracker)
  obj_conversion <- gWidgets2::gedit("",
                          label = "mm conversion factor",
                          container = form_eyetracker)
  ########

  ## Message Markers ####
  frame_message <- gWidgets2::gframe(" Message Marker Information ",
                                     horizontal = FALSE,
                                     container = grp_2, pos = .5)
  form_message <- gWidgets2::gformlayout(container = frame_message,
                                         expand = TRUE)
  obj_start_tracking.message <- gWidgets2::gedit("default",
                                      label = "start tracking message",
                                      container = form_message)
  obj_start_tracking.match <- gWidgets2::gcombobox(c("exact", "pattern"),
                                                   select = 1,
                                                   label = "string match type",
                                                   container = form_message)
  obj_trial_onset.message <- gWidgets2::gedit("",
                                   label = "trial onset message",
                                   container = form_message)
  obj_trial_onset.match <- gWidgets2::gcombobox(c("exact", "pattern"),
                                                select = 1,
                                                label = "string match type",
                                                container = form_message)
  obj_pretrial.duration <- gWidgets2::gedit("",
                                  label = "pre trial duration",
                                  container = form_message)
  obj_bc_onset.message <- gWidgets2::gedit("",
                                label = "baseline correction onset message",
                                container = form_message)
  obj_bc_onset.match <- gWidgets2::gcombobox(c("exact", "pattern"),
                                             select = 1,
                                             label = "string match type",
                                             container = form_message)
  #######

  ## Third Row - Preprocessing
  grp_3 <- gWidgets2::ggroup(horizontal = TRUE, container = grp_all)

  ## Preprocessing ####
  frame_preprocessing <- gWidgets2::gframe(" Preprocessing Parameters ",
                                           horizontal = FALSE,
                                           container = grp_3, pos = .5)
  form_preprocessing <- gWidgets2::gformlayout(container = frame_preprocessing,
                                               expand = TRUE)
  obj_deblink.extend <- gWidgets2::gedit("",
                              label = "deblink extension (ms)",
                              container = form_preprocessing)
  obj_smooth <- gWidgets2::gcombobox(c("hann", "mwa"), select = 1,
                          label = "smoothing/filtering method",
                          container = form_preprocessing)
  obj_smooth.window <- gWidgets2::gedit(500,
                             label = "smooth window size (ms)",
                             container = form_preprocessing)
  obj_interpolate <- gWidgets2::gcombobox(c("cubic-spline", "linear"),
                                          select = 1,
                                          label = "interpolation method",
                                          container = form_preprocessing)
  obj_interpolate.maxgap <-
    gWidgets2::gedit(Inf,
                     label = "maximum gap to interpolate over (ms)",
                     container = form_preprocessing)
  obj_method.first <-
    gWidgets2::gcombobox(c("smooth", "interpolate"),
                         select = 1,
                         label = "smooth or interpolate first?",
                         container = form_preprocessing)
  obj_bc <- gWidgets2::gcombobox(c("subtractive", "divisive"), select = 1,
                      label = "baseline correction method",
                      container = form_preprocessing)
  obj_pre_bc.duration <- gWidgets2::gedit("",
                               label = "duration of baseline (ms)",
                               container = form_preprocessing)
  obj_missing.allowed <- gWidgets2::gedit("",
                               label = "amount of missing values allowed",
                               container = form_preprocessing)
  ########

  ## Execute ####
  grp_4 <- gWidgets2::ggroup(horizontal = FALSE, container = grp_win)
  frame_execute <- gWidgets2::gframe("", horizontal = FALSE,
                          container = grp_4, pos = .5)
  button_execute <-
    gWidgets2::gbutton(
      "Execute Preprocessing",
      container = frame_execute,
      handler = function(h, ...){
        if (gWidgets2::svalue(obj_timing.file) == "") {
          gWidgets2::svalue(obj_timing.file) <- NULL
        }
        if (gWidgets2::svalue(obj_subj.prefix) == ""){
          gWidgets2::svalue(obj_subj.prefix) <- NULL
        }
        if (gWidgets2::svalue(obj_subj.suffix) == ""){
          gWidgets2::svalue(obj_subj.suffix) <- NULL
        }
        if (gWidgets2::svalue(obj_conversion) == ""){
          gWidgets2::svalue(obj_conversion) <- NULL
        }
        if (gWidgets2::svalue(obj_include_col) == ""){
          gWidgets2::svalue(obj_include_col) <- NULL
        }
        if (gWidgets2::svalue(obj_trial.exclude) == ""){
          gWidgets2::svalue(obj_trial.exclude) <- NULL
        }

        print((gWidgets2::svalue(obj_timing.file) != ""))

        pupillometry::pupil_preprocess(
          import_dir = gWidgets2::svalue(obj_import.dir),
          pattern = gWidgets2::svalue(obj_pattern),
          taskname = gWidgets2::svalue(obj_taskname),
          subj_prefix = gWidgets2::svalue(obj_subj.prefix),
          subj_suffix = gWidgets2::svalue(obj_subj.suffix),
          timing_file = gWidgets2::svalue(obj_timing.file),
          output_dir = gWidgets2::svalue(obj_output.dir),
          output_steps = gWidgets2::svalue(obj_output.steps),
          files_merge = gWidgets2::svalue(obj_files.merge),
          eyetracker = gWidgets2::svalue(obj_eyetracker),
          hz = as.numeric(gWidgets2::svalue(obj_hz)),
          eye_use = gWidgets2::svalue(obj_eye.use),
          px_to_mm.conversion = as.numeric(gWidgets2::svalue(obj_conversion)),
          start_tracking.message = gWidgets2::svalue(obj_start_tracking.message),
          start_tracking.match = gWidgets2::svalue(obj_start_tracking.match),
          trial_onset.message = gWidgets2::svalue(obj_trial_onset.message),
          trial_onset.match = gWidgets2::svalue(obj_trial_onset.match),
          pretrial.duration = as.numeric(gWidgets2::svalue(obj_pretrial.duration)),
          bc_onset.message = gWidgets2::svalue(obj_bc_onset.message),
          bc_onset.match = gWidgets2::svalue(obj_bc_onset.match),
          deblink_extend = as.numeric(gWidgets2::svalue(obj_deblink.extend)),
          smooth = gWidgets2::svalue(obj_smooth),
          smooth.window = as.numeric(gWidgets2::svalue(obj_smooth.window)),
          interpolate = gWidgets2::svalue(obj_interpolate),
          interpolate.maxgap = as.numeric(gWidgets2::svalue(obj_interpolate.maxgap)),
          method_first = gWidgets2::svalue(obj_method.first),
          pre_bc.duration = as.numeric(gWidgets2::svalue(obj_pre_bc.duration)),
          missing_allowed = as.numeric(gWidgets2::svalue(obj_missing.allowed)),
          include_col = gWidgets2::svalue(obj_include_col),
          trial_exclude = as.numeric(gWidgets2::svalue(obj_trial.exclude)))
                         })

  button_rcode <-
    gWidgets2::gbutton("View R Code", container = frame_execute,
                       handler = function(h, ...){
                         if (gWidgets2::svalue(obj_timing.file) == "") {
                           gWidgets2::svalue(obj_timing.file) <- "NULL"
                          }
                         if (gWidgets2::svalue(obj_subj.prefix) == ""){
                           gWidgets2::svalue(obj_subj.prefix) <- "NULL"
                          }
                         if (gWidgets2::svalue(obj_subj.suffix) == ""){
                           gWidgets2::svalue(obj_subj.suffix) <- "NULL"
                          }
                         if (gWidgets2::svalue(obj_conversion) == ""){
                           gWidgets2::svalue(obj_conversion) <- "NULL"
                          }
                         if (gWidgets2::svalue(obj_include_col) == ""){
                           gWidgets2::svalue(obj_include_col) <- "NULL"
                          }
                         if (gWidgets2::svalue(obj_trial.exclude) == ""){
                           gWidgets2::svalue(obj_trial.exclude) <- "NULL"
                          }

                         gWidgets2::gtext(container = gWidgets2::gwindow(),
                                  do.newline = TRUE,
                                  text =
                                    stringr::str_replace(
                                      paste("## Preprocessing Parameters\n",
                                            " \n",
                                            "# File Import Parameters\n",
                                            "import_dir <- ", "\"",
                                            gWidgets2::svalue(obj_import.dir),
                                            "\"", "\n",
                                            "pattern <- ", "\"",
                                            gWidgets2::svalue(obj_pattern),
                                            "\"", "\n",
                                            "taskname <- ", "\"",
                                            gWidgets2::svalue(obj_taskname),
                                            "\"", "\n",
                                            "timing_file <- ", "\"",
                                            gWidgets2::svalue(obj_timing.file),
                                            "\"", "\n",
                                            "subj_prefix <- ", "\"",
                                            gWidgets2::svalue(obj_subj.prefix),
                                            "\"", "\n",
                                            "subj_suffix <- ", "\"",
                                            gWidgets2::svalue(obj_subj.suffix),
                                            "\"", "\n", " \n",
                                            "# File Output Parameters\n",
                                            "output_dir <- ", "\"",
                                            gWidgets2::svalue(obj_output.dir),
                                            "\"", "\n",
                                            "output_steps <- ",
                                            gWidgets2::svalue(obj_output.steps),
                                            "\n",
                                            "files_merge <- ",
                                            gWidgets2::svalue(obj_files.merge),
                                            "\n", " \n",
                                            "# Eyetracker Information\n",
                                            "eyetracker <- ", "\"",
                                            gWidgets2::svalue(obj_eyetracker),
                                            "\"", "\n",
                                            "hz <- ",
                                            gWidgets2::svalue(obj_hz), "\n",
                                            "eye_use <- ", "\"",
                                            gWidgets2::svalue(obj_eye.use),
                                            "\"", "\n",
                                            "px_to_mm.conversion <- ",
                                            gWidgets2::svalue(obj_conversion),
                                            "\n", " \n",
                                            "# Message Marker Parameters\n",
                                            "start_tracking.message <- ", "\"",
                                            gWidgets2::svalue(
                                              obj_start_tracking.message),
                                            "\"", "\n",
                                            "start_tracking.match <- ", "\"",
                                            gWidgets2:: svalue(
                                              obj_start_tracking.match),
                                            "\"", "\n",
                                            "trial_onset.message <- ", "\"",
                                            gWidgets2::svalue(
                                              obj_trial_onset.message),
                                            "\"", "\n",
                                            "trial_onset.match <- ", "\"",
                                            gWidgets2::svalue(
                                              obj_trial_onset.match),
                                            "\"", "\n",
                                            "pretrial.duration <- ",
                                            gWidgets2::svalue(
                                              obj_pretrial.duration), "\n",
                                            "bc_onset.message <- ", "\"",
                                            gWidgets2::svalue(
                                              obj_bc_onset.message),
                                            "\"", "\n",
                                            "bc_onset.match <- ", "\"",
                                            gWidgets2::svalue(
                                              obj_bc_onset.match), "\"", "\n",
                                            " \n",
                                            "# Preprocessing Parameters\n",
                                            "deblink_extend <- ",
                                            gWidgets2::svalue(
                                              obj_deblink.extend), "\n",
                                            "smooth <- ", "\"",
                                            gWidgets2::svalue(obj_smooth),
                                            "\"", "\n",
                                            "smooth.window <- ",
                                            gWidgets2::svalue(
                                              obj_smooth.window), "\n",
                                            "interpolate <- ", "\"",
                                            gWidgets2::svalue(obj_interpolate),
                                            "\"", "\n",
                                            "interpolate.maxgap <- ",
                                            gWidgets2::svalue(
                                              obj_interpolate.maxgap), "\n",
                                            "method_first <- ", "\"",
                                            gWidgets2::svalue(obj_method.first),
                                            "\"", "\n",
                                            "bc <- ", "\"",
                                            gWidgets2::svalue(obj_bc),
                                            "\"", "\n",
                                            "pre_bc.duration <- ",
                                            gWidgets2::svalue(
                                              obj_pre_bc.duration), "\n",
                                            "missing_allowed <- ",
                                            gWidgets2::svalue(
                                              obj_missing.allowed), "\n",
                                            " \n",
                                            "# Misc.\n",
                                            "include_col <- ", "\"",
                                            gWidgets2::svalue(obj_include_col),
                                            "\"", "\n",
                                            "trial_exclude <- ",
                                            gWidgets2::svalue(
                                              obj_trial.exclude), "\n",
                                            " \n",
                                            "#############################",
                                            " \n",
                                            "pupil_preprocess(import_dir = import_dir, pattern = pattern, taskname = taskname,
                  subj_prefix = subj_prefix, subj_suffix = subj_suffix,
                  timing_file = timing_file, output_dir = output_dir,
                  output_steps = output_steps, files_merge = files_merge,
                  eyetracker = eyetracker, hz = hz, eye_use = eye_use,
                  px_to_mm.conversion = px_to_mm.conversion,
                  start_tracking.message = start_tracking.message,
                  start_tracking.match = start_tracking.match,
                  trial_onset.message = trial_onset.message,
                  trial_onset.match = trial_onset.match,
                  pretrial.duration = pretrial.duration,
                  bc_onset.message = bc_onset.message,
                  bc_onset.match = bc_onset.match,
                  deblink_extend = deblink_extend,
                  smooth = smooth, smooth.window = smooth.window,
                  interpolate = interpolate,
                  interpolate.maxgap = interpolate.maxgap,
                  method_first = method_first, bc = bc,
                  pre_bc.duration = pre_bc.duration,
                  missing_allowed = missing_allowed, include_col = include_col,
                  trial_exclude = trial_exclude)",
                                                      sep = ""),
                                      "\"NULL\"", "NULL"))
                          })

  button_help <- gWidgets2::gbutton("help", container = frame_execute,
                                    handler = function(h,...) {
                                      utils::browseURL("https://dr-jt.github.io/pupillometry/articles/gui.html")
                                    })
  ########

  gWidgets2::visible(win) <- TRUE
}

