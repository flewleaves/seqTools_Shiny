# utils/ai.R

call_ai <- function(prompt, config) {
  
  provider <- config$ai$provider
  key <- config$ai$key
  model_id <- config$ai$model
  base_url <- config$ai$base_url
  
  if (is.null(key) || key == "") stop("API Key 未设置")
  
  model <- switch(provider,
    "openai" = {
      if (!is.null(base_url) && base_url != "") {
        openai$language_model(model_id, api_key = key, base_url = base_url)
      } else {
        openai$language_model(model_id, api_key = key)
      }
    },
    "deepseek" = create_deepseek(api_key = key)$language_model(model_id),
    "aliyun"   = create_aliyun(api_key = key)$language_model(model_id),
    "custom"   = openai$language_model(model_id, api_key = key, base_url = base_url),
    stop("不支持的提供商: ", provider)
  )
  
  generate_text(model, prompt)$text
}