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
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import javafx.scene.image.ImageView;

import javax.imageio.ImageIO;
import javax.inject.Inject;

import org.pdfsam.context.DefaultUserContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Bean;
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
@PropertySource("classpath:pdfsam.properties")
public class PdfsamConfig {
    private static final Logger LOG = LoggerFactory.getLogger(PdfsamConfig.class);
    @Inject
    private Environment env;

    @Bean(name = "pdfsamLogoImage")
    public Image getAppImage() throws IOException {
        Resource resource = new ClassPathResource("/images/pdfsam_" + env.getProperty("pdfsam.package", "BASIC")
                + ".png");
        return ImageIO.read(resource.getInputStream());
    }

    @Bean
    public ImageView payoff() throws IOException {
        return new ImageView(new ClassPathResource("/images/payoff.png").getURL().toExternalForm());
    }

    @Bean(name = "appVersion")
    public String version() {
        return env.getProperty("pdfsam.version");
    }

    @Bean(name = "styles")
    public List<String> styles() {
        List<String> styles = new ArrayList<>();
        styles.add(this.getClass().getResource("/css/defaults.css").toExternalForm());
        styles.add(this.getClass().getResource("/css/pdfsam.css").toExternalForm());
        styles.add(this.getClass().getResource("/css/menu.css").toExternalForm());
        try {
            URL themeUrl = new ClassPathResource("/css/themes/" + DefaultUserContext.getInstance().getTheme()).getURL();
            styles.add(themeUrl.toExternalForm());
        } catch (IOException ioe) {
            LOG.warn("Unable to find selected theme.", ioe);
        }
        return styles;
    }
}
