source("model_registry.R")

.settings <- load_model_settings("settings.json")
register_models(.settings)

.model_choices <- setNames(vapply(.settings, `[[`, "", "id"),
                           vapply(.settings, `[[`, "", "label"))
.model_choices
.settings[[1]]
