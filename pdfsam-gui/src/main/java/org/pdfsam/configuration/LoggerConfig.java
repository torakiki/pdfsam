/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 01/dic/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as 
 * published by the Free Software Foundation, either version 3 of the 
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.configuration;

import javafx.scene.control.TextArea;

import org.pdfsam.ui.log.LogPane;
import org.pdfsam.ui.log.TextAreaAppender;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.ComponentScan.Filter;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.FilterType;

import ch.qos.logback.classic.encoder.PatternLayoutEncoder;

/**
 * IoC configuration for the loggin system
 * 
 * @author Andrea Vacondio
 * 
 */
@Configuration
@ComponentScan(basePackages = { "org.pdfsam.gui.log" }, includeFilters = @Filter(type = FilterType.ASSIGNABLE_TYPE, value = {
        LogPane.class, TextAreaAppender.class }))
public class LoggerConfig {

    @Bean(name = "logArea")
    public TextArea logArea() {
        TextArea logArea = new TextArea();
        logArea.setEditable(false);
        logArea.setWrapText(true);
        return logArea;
    }

    @Bean
    public PatternLayoutEncoder pattern() {
        PatternLayoutEncoder encoder = new PatternLayoutEncoder();
        encoder.setPattern("[%d{HH:mm:ss}] %msg%n");
        return encoder;
    }
}
