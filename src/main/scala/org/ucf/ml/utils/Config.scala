package org.ucf.ml.utils

import org.ucf.ml.utils

class Config (configPath:String = "src/main/resources/default_application.conf") extends utils.PropertiesLoader(configPath)
