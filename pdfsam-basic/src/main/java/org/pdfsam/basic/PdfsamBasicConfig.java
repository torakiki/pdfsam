/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 22/ott/2013
 * Copyright 2017 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
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
package org.pdfsam.basic;

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
 * Configuration for PDFsam Basic Edition
 * 
 * @author Andrea Vacondio
 * 
 */
public class PdfsamBasicConfig {

    public Image logo16() {
        return new Image(this.getClass().getResourceAsStream("/images/basic/16x16.png"));
    }

    public Image logo24() {
        return new Image(this.getClass().getResourceAsStream("/images/basic/24x24.png"));
    }

    @Provides
    @Named("logo32")
    @Prototype
    public Image logo32() {
        return new Image(this.getClass().getResourceAsStream("/images/basic/32x32.png"));
    }

    @Provides
    @Named("logo48")
    @Prototype
    public Image logo48() {
        return new Image(this.getClass().getResourceAsStream("/images/basic/48x48.png"));
    }

    public Image logo64() {
        return new Image(this.getClass().getResourceAsStream("/images/basic/64x64.png"));
    }

    public Image logo96() {
        return new Image(this.getClass().getResourceAsStream("/images/basic/96x96.png"));
    }

    @Provides
    @Named("logo128")
    @Prototype
    public Image logo128() {
        return new Image(this.getClass().getResourceAsStream("/images/basic/128x128.png"));
    }

    public Image logo256() {
        return new Image(this.getClass().getResourceAsStream("/images/basic/256x256.png"));
    }

    public Image logo512() {
        return new Image(this.getClass().getResourceAsStream("/images/basic/512x512.png"));
    }

    @Provides
    @Auto
    public Pdfsam pdfsam() throws IOException {
        return new PdfsamBasic("PDF Split and Merge Basic Edition", "PDFsam Basic");
    }

    @Provides
    @Named("updatesUrl")
    public Object updatesUrl(Pdfsam pdfsam) throws MalformedURLException {
        return new URL(String.format("http://www.pdfsam.org/current-version?c=%s",
                pdfsam.property(ConfigurableProperty.VERSION)));
    }
}
