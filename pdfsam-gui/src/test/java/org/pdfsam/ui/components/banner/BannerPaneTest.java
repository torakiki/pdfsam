/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 21/ago/2014
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
package org.pdfsam.ui.components.banner;

import static org.junit.Assert.assertTrue;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

import java.net.URISyntaxException;

import javax.inject.Named;

import org.junit.Rule;
import org.junit.Test;
import org.pdfsam.injector.Injector;
import org.pdfsam.injector.Prototype;
import org.pdfsam.injector.Provides;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.ui.components.event.SetTitleEvent;
import org.testfx.framework.junit.ApplicationTest;
import org.testfx.util.WaitForAsyncUtils;

import javafx.scene.Scene;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.stage.Stage;

/**
 * @author Andrea Vacondio
 *
 */
public class BannerPaneTest extends ApplicationTest {
    @Rule
    public ClearEventStudioRule cleanStudio = new ClearEventStudioRule();

    @Override
    public void start(Stage stage) {
        Injector injector = Injector.start(new MenuConfig(), new Config());
        Scene scene = new Scene(injector.instance(BannerPane.class));
        stage.setScene(scene);
        stage.show();
    }

    static class Config {
        @Provides
        public BannerPane victim(BannerButtons buttons, @Named("logo32") Image logo, ImageView payoff) {
            return new BannerPane(buttons, payoff, logo);
        }

        @Provides
        @Prototype
        public ImageView payoff() throws URISyntaxException {
            return new ImageView(this.getClass().getResource("/images/payoff.png").toURI().toString());
        }

        @Provides
        @Named("logo32")
        @Prototype
        public Image logo32() {
            return new Image(this.getClass().getResourceAsStream("/images/payoff.png"));
        }

        @Provides
        public BannerButtons buttons(LogButton logButton, DashboardButton dashboardButton, NewsButton newsButton,
                MenuButton menuButton) {
            return new BannerButtons(logButton, dashboardButton, newsButton, menuButton);
        }

        @Provides
        public DashboardButton dashboardButton() {
            return new DashboardButton("itemId");
        }

        @Provides
        public NewsButton newsButton() {
            return new NewsButton();
        }

    }

    @Test
    public void title() {
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> eventStudio().broadcast(new SetTitleEvent("title")));
        assertTrue(lookup("@title").tryQuery().isPresent());
    }

}
