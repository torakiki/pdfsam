/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 24 ott 2015
 * Copyright 2013-2014 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.news;

import java.io.IOException;
import java.net.URL;
import java.util.Collections;
import java.util.List;
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;

import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.ConfigurableProperty;
import org.pdfsam.Pdfsam;
import org.pdfsam.i18n.DefaultI18nContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.jr.ob.JSON;
import com.fasterxml.jackson.jr.ob.JSON.Feature;

/**
 * Default JSON implementation of a news service
 * 
 * @author Andrea Vacondio
 */
@Named
class DefaultNewsService implements NewsService {

    private static final Logger LOG = LoggerFactory.getLogger(DefaultNewsService.class);
    private static final String NEWS_PATH = "/org/pdfsam/user/news";
    private static final String LATEST_NEWS_ID = "latest.news.id";
    private Pdfsam pdfsam;

    @Inject
    DefaultNewsService(Pdfsam pdfsam) {
        this.pdfsam = pdfsam;
    }

    public List<NewsData> getLatestNews() {
        try {
            return JSON.std.with(Feature.READ_ONLY, true).listOfFrom(NewsData.class,
                    new URL(pdfsam.property(ConfigurableProperty.NEWS_URL)));
        } catch (IOException e) {
            LOG.warn(DefaultI18nContext.getInstance().i18n("Unable to retrieve latest news"), e);
        }
        return Collections.emptyList();
    }

    public void setLatestNewsSeen(int id) {
        Preferences.userRoot().node(NEWS_PATH).putInt(LATEST_NEWS_ID, id);
        LOG.trace("Latest news id stored");
    }

    public int getLatestNewsSeen() {
        return Preferences.userRoot().node(NEWS_PATH).getInt(LATEST_NEWS_ID, -1);
    }

    public void clear() {
        Preferences prefs = Preferences.userRoot().node(NEWS_PATH);
        try {
            prefs.removeNode();
            prefs.flush();
        } catch (BackingStoreException e) {
            LOG.error("Unable to clear latest news store", e);
        }
    }
}
