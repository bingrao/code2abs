package org.ucf.ml

class Config (configPath:String = "src/main/resources/default_application.conf") extends utils.PropertiesLoader(configPath)
