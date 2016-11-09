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
import java.net.MalformedURLException;
import java.net.URL;

import javax.inject.Named;

import org.pdfsam.ConfigurableProperty;
import org.pdfsam.Pdfsam;
import org.sejda.injector.Auto;
import org.sejda.injector.Prototype;
import org.sejda.injector.Provides;

import javafx.scene.image.Image;

/**
 * Configuration for PDFsam Community Edition
 * 
 * @author Andrea Vacondio
 * 
 */
public class PdfsamCommunityConfig {

    public Image logo16() {
        return new Image(this.getClass().getResourceAsStream("/images/community/16x16.png"));
    }

    public Image logo24() {
        return new Image(this.getClass().getResourceAsStream("/images/community/24x24.png"));
    }

    @Provides
    @Named("logo32")
    @Prototype
    public Image logo32() {
        return new Image(this.getClass().getResourceAsStream("/images/community/32x32.png"));
    }

    @Provides
    @Named("logo48")
    @Prototype
    public Image logo48() {
        return new Image(this.getClass().getResourceAsStream("/images/community/48x48.png"));
    }

    public Image logo64() {
        return new Image(this.getClass().getResourceAsStream("/images/community/64x64.png"));
    }

    public Image logo96() {
        return new Image(this.getClass().getResourceAsStream("/images/community/96x96.png"));
    }

    @Provides
    @Named("logo128")
    @Prototype
    public Image logo128() {
        return new Image(this.getClass().getResourceAsStream("/images/community/128x128.png"));
    }

    public Image logo256() {
        return new Image(this.getClass().getResourceAsStream("/images/community/256x256.png"));
    }

    public Image logo512() {
        return new Image(this.getClass().getResourceAsStream("/images/community/512x512.png"));
    }

    @Provides
    @Auto
    public Pdfsam pdfsam() throws IOException {
        return new PdfsamCommunity("PDF Split and Merge Basic Edition", "PDFsam Basic");
    }

    @Provides
    @Named("updatesUrl")
    public Object updatesUrl(Pdfsam pdfsam) throws MalformedURLException {
        return new URL(String.format("http://www.pdfsam.org/current-version?c=%s",
                pdfsam.property(ConfigurableProperty.VERSION)));
    }
}
