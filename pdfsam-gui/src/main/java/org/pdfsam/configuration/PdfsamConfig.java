/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 13/dic/2011
 * Copyright 2011 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URL;

import javax.inject.Inject;

import org.pdfsam.context.DefaultUserContext;
import org.pdfsam.context.UserContext;
import org.pdfsam.ui.Theme;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;
import org.springframework.core.env.Environment;
import org.springframework.core.io.ClassPathResource;

import javafx.scene.image.ImageView;

/**
 * IoC configuration
 * 
 * @author Andrea Vacondio
 * 
 */
@Configuration
@PropertySource("classpath:pdfsam.properties")
public class PdfsamConfig {
    private static final Logger LOG = LoggerFactory.getLogger(PdfsamConfig.class);
    @Inject
    private Environment env;

    @Bean
    public ImageView payoff() throws IOException {
        return new ImageView(new ClassPathResource("/images/payoff.png").getURL().toExternalForm());
    }

    @Bean(name = "updatesUrl")
    public URL updatesUrl() throws MalformedURLException {
        return new URL(String.format("http://www.pdfsam.org/current-version?c=%s", env.getProperty("pdfsam.version")));
    }

    @Bean
    public UserContext userContext() {
        return new DefaultUserContext();
    }

    @Bean
    public StylesConfig styles() {
        String themeString = userContext().getTheme();
        Theme selected = Theme.ROUNDISH;
        try {
            selected = Theme.valueOf(themeString);
        } catch (IllegalArgumentException e) {
            LOG.warn("Unable to find selected theme: {}.", themeString);
        }
        return new StylesConfig(selected);
    }

    @Bean(name = "errorSound")
    public String error() throws URISyntaxException {
        return this.getClass().getResource("/sounds/error_sound.wav").toURI().toString();
    }

    @Bean(name = "okSound")
    public String ok() throws URISyntaxException {
        return this.getClass().getResource("/sounds/ok_sound.wav").toURI().toString();
    }

}
