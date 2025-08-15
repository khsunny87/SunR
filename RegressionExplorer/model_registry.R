# WHERE: model_registry.R (new file at project root)
# WHY: settings.json을 읽어 각 모델 모듈을 로드/등록하고 app.R에서 공통 인터페이스로 호출 가능하게 함
# <<<INSERT START>>>
suppressPackageStartupMessages({
  library(jsonlite)
  library(rlang)
})

ModelRegistry <- new.env(parent = baseenv())

`%||%` <- function(x, y) if (is.null(x)) y else x

load_model_settings <- function(path = "settings.json") {
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

register_models <- function(settings) {
  # 초기화
  rm(list = ls(ModelRegistry), envir = ModelRegistry)
  for (s in settings) {
    if (!file.exists(s$file)) stop("Model file not found: ", s$file)
    # 각 모델 파일은 .model_<id> 객체를 정의해야 함
    sys.source(s$file, envir = ModelRegistry)
    obj_name <- paste0(".model_", s$id)
    if (!exists(obj_name, envir = ModelRegistry, inherits = FALSE)) {
      stop("Model object not exported: ", obj_name)
    }
    mod <- get(obj_name, envir = ModelRegistry, inherits = FALSE)
    
    # settings 메타 주입(모듈에 명시된 값이 우선)
    mod$id            <- mod$id            %||% s$id
    mod$label         <- mod$label         %||% s$label
    mod$time_based    <- mod$time_based    %||% s$time_based
    mod$outcome_categ <- mod$outcome_categ %||% s$outcome_categorical
    mod$effect_name   <- mod$effect_name   %||% s$effect_name
    mod$supports_step <- mod$supports_step %||% isTRUE(s$supports_stepwise)
    mod$step_impl     <- mod$step_impl     %||% s$step_impl
    
    assign(s$id, mod, envir = ModelRegistry)
  }
  invisible(TRUE)
}

get_model <- function(id) {
  if (!exists(id, envir = ModelRegistry, inherits = FALSE)) {
    stop("Model not registered: ", id)
  }
  get(id, envir = ModelRegistry, inherits = FALSE)
}

# 공통 스텝와이즈 실행기
run_stepwise <- function(mod, base_fit, lower_form, upper_form, k, direction) {
  if (!isTRUE(mod$supports_step) || is.null(direction) || direction == "none") return(base_fit)
  impl <- mod$step_impl %||% "none"
  if (impl == "stats_step") {
    stats::step(
      base_fit,
      scope     = list(lower = lower_form, upper = upper_form),
      direction = direction,
      k         = k,
      trace     = 0
    )
  } else if (impl == "stepAIC") {
    MASS::stepAIC(
      base_fit,
      scope     = list(lower = lower_form, upper = upper_form),
      direction = direction,
      k         = k,
      trace     = FALSE
    )
  } else {
    base_fit
  }
}
# <<<INSERT END>>>