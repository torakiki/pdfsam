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
package org.pdfsam.community;

import java.io.IOException;

import javax.inject.Inject;

import org.pdfsam.Pdfsam;
import org.pdfsam.configuration.UIConfig;
import org.pdfsam.module.PdfsamModule;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.ComponentScan.Filter;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.core.env.Environment;
import org.springframework.core.io.ClassPathResource;

import javafx.scene.image.Image;

/**
 * Configuration for PDFsam Community Edition
 * 
 * @author Andrea Vacondio
 * 
 */
@Configuration
@Profile("COMMUNITY")
@ComponentScan(basePackages = { "org.pdfsam" }, includeFilters = @Filter(value = PdfsamModule.class) )
public class PdfsamCommunityConfig implements UIConfig {

    @Inject
    private Environment env;

    @Bean(name = "logo16")
    public Image logo16() throws IOException {
        return new Image(new ClassPathResource("/images/community/16x16.png").getInputStream());
    }

    @Bean(name = "logo24")
    public Image logo24() throws IOException {
        return new Image(new ClassPathResource("/images/community/24x24.png").getInputStream());
    }

    @Bean(name = "logo32")
    public Image logo32() throws IOException {
        return new Image(new ClassPathResource("/images/community/32x32.png").getInputStream());
    }

    @Bean(name = "logo48")
    public Image logo48() throws IOException {
        return new Image(new ClassPathResource("/images/community/48x48.png").getInputStream());
    }

    @Bean(name = "logo64")
    public Image logo64() throws IOException {
        return new Image(new ClassPathResource("/images/community/64x64.png").getInputStream());
    }

    @Bean(name = "logo96")
    public Image logo96() throws IOException {
        return new Image(new ClassPathResource("/images/community/96x96.png").getInputStream());
    }

    @Bean(name = "logo128")
    public Image logo128() throws IOException {
        return new Image(new ClassPathResource("/images/community/128x128.png").getInputStream());
    }

    @Bean(name = "logo256")
    public Image logo256() throws IOException {
        return new Image(new ClassPathResource("/images/community/256x256.png").getInputStream());
    }

    @Bean(name = "logo512")
    public Image logo512() throws IOException {
        return new Image(new ClassPathResource("/images/community/512x512.png").getInputStream());
    }

    @Override
    @Bean
    public Pdfsam pdfsam() {
        return new PdfsamCommunity("PDF Split and Merge Basic Edition", "PDFsam Basic", env);
    }
}
