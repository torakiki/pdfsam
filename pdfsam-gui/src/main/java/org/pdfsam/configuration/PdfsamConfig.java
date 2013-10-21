/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 13/dic/2011
 * Copyright 2011 by Andrea Vacondio (andrea.vacondio@gmail.com).
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

import java.awt.Image;
import java.io.IOException;

import javafx.scene.Scene;

import javax.imageio.ImageIO;
import javax.inject.Inject;

import org.pdfsam.context.DefaultUserContext;
import org.pdfsam.gui.log.LogPane;
import org.pdfsam.module.PdfsamModule;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.ComponentScan.Filter;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;
import org.springframework.core.env.Environment;
import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.Resource;

/**
 * IoC configuration
 * 
 * @author Andrea Vacondio
 * 
 */
@Configuration
@ComponentScan(basePackages = { "org.pdfsam" }, includeFilters = @Filter(value = PdfsamModule.class))
@PropertySource("classpath:pdfsam.properties")
public class PdfsamConfig {

    @Inject
    private Environment env;

    @Bean(name = "pdfsamLogoImage")
    public Image getAppImage() throws IOException {
        Resource resource = new ClassPathResource("/images/pdfsam_" + env.getProperty("pdfsam.package", "BASIC")
                + ".png");
        return ImageIO.read(resource.getInputStream());
    }

    @Bean(name = "logScene")
    public Scene logScene() {
        Scene scene = new Scene(new LogPane());
        scene.getStylesheets().addAll(styles());
        return scene;
    }

    public String[] styles() {
        String css1 = this.getClass().getResource("/css/default.css").toExternalForm();
        String css2 = this.getClass().getResource("/css/" + DefaultUserContext.getInstance().getTheme())
                .toExternalForm();
        return new String[] { css1, css2 };
    }
}
