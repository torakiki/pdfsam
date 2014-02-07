/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 22/ott/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.configuration;

import java.io.IOException;

import javafx.fxml.FXMLLoader;
import javafx.scene.Group;
import javafx.scene.image.Image;

import org.pdfsam.module.PdfsamModule;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.ComponentScan.Filter;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.Resource;

/**
 * UI Configuration for the Basic version
 * 
 * @author Andrea Vacondio
 * 
 */
@Configuration
@Profile("BASIC")
@ComponentScan(basePackages = { "org.pdfsam" }, includeFilters = @Filter(value = PdfsamModule.class))
public class PdfsamBasicConfig implements UIConfig {

    @Bean(name = "logo")
    public Group logo() throws IOException {
        Resource resource = new ClassPathResource("/fxml/LogoBasic.fxml");
        return FXMLLoader.load(resource.getURL());
    }

    @Bean(name = "logo80")
    public Image logo80() throws IOException {
        Resource resource = new ClassPathResource("/images/logo80B.png");
        return new Image(resource.getInputStream());
    }

    @Bean(name = "logo35")
    public Image logo35() throws IOException {
        Resource resource = new ClassPathResource("/images/logo35B.png");
        return new Image(resource.getInputStream());
    }

    @Bean(name = "appName")
    public String appName() {
        return "PDF Split And Merge Basic";
    }
}
