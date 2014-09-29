/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 22/ott/2013
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

import java.io.IOException;

import javafx.fxml.FXMLLoader;
import javafx.scene.Group;
import javafx.scene.image.Image;

import org.pdfsam.module.PdfsamEnhancedModule;
import org.pdfsam.module.PdfsamModule;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.ComponentScan.Filter;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.Resource;

/**
 * UI Configuration for the Enhanced version
 * 
 * @author Andrea Vacondio
 * 
 */
@Configuration
@Profile("ENHANCED")
@ComponentScan(basePackages = { "org.pdfsam" }, includeFilters = @Filter(value = { PdfsamModule.class,
        PdfsamEnhancedModule.class }))
public class PdfsamEnhancedConfig implements UIConfig {

    @Bean(name = "logo")
    public Group logo() throws IOException {
        Resource resource = new ClassPathResource("/fxml/LogoEnhanced.fxml");
        return FXMLLoader.load(resource.getURL());
    }

    @Bean(name = "logo80")
    public Image logo80() throws IOException {
        return new Image(new ClassPathResource("/images/logo80E.png").getInputStream());
    }

    @Bean(name = "logo35")
    public Image logo35() throws IOException {
        return new Image(new ClassPathResource("/images/logo35E.png").getInputStream());
    }

    @Bean(name = "logo16")
    public Image logo16() throws IOException {
        return new Image(new ClassPathResource("/images/logo16E.png").getInputStream());
    }

    @Bean(name = "logo32")
    public Image logo32() throws IOException {
        return new Image(new ClassPathResource("/images/logo32E.png").getInputStream());
    }

    @Bean(name = "logo64")
    public Image logo64() throws IOException {
        return new Image(new ClassPathResource("/images/logo64E.png").getInputStream());
    }

    @Bean(name = "logo128")
    public Image logo128() throws IOException {
        return new Image(new ClassPathResource("/images/logo128E.png").getInputStream());
    }

    @Bean(name = "logo256")
    public Image logo256() throws IOException {
        return new Image(new ClassPathResource("/images/logo256E.png").getInputStream());
    }

    @Bean(name = "logo512")
    public Image logo512() throws IOException {
        return new Image(new ClassPathResource("/images/logo512E.png").getInputStream());
    }

    @Bean(name = "logo1024")
    public Image logo1024() throws IOException {
        return new Image(new ClassPathResource("/images/logo1024E.png").getInputStream());
    }

    @Bean(name = "appName")
    public String appName() {
        return "PDF Split And Merge Enhanced";
    }
}
